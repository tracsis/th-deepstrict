{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE TemplateHaskellQuotes      #-}
{-# LANGUAGE TupleSections              #-}

-- | Check that a datatype is deeply strict, ie, it recursively only has strict fields.
module Language.Haskell.TH.DeepStrict
  (
  -- * DeepStrict
    DeepStrict(..)
  , DeepStrictReason(..)
  , DeepStrictWithReason
  -- * Checking data types
  , isDeepStrict
  , isDeepStrictWith
  , assertDeepStrict
  , assertDeepStrictWith
  -- * Context
  , Context(..)
  , ExpectedStrictness(..)
  , emptyContext
  , FieldKey
  ) where

import Data.Maybe                    (mapMaybe, fromMaybe)
import Data.List                     (foldl')
import Control.Monad                 (when)
import Control.Monad.IO.Class        (MonadIO)
import Control.Monad.Reader          (MonadReader (ask, local), ReaderT (..), asks)
import Control.Monad.Trans           (lift)
import Data.IORef                    (IORef, modifyIORef', newIORef, readIORef)
import Data.Traversable              (for)
import GHC.Stack                     (HasCallStack)
import Language.Haskell.TH           (Q)
import Language.Haskell.TH.Instances ()
import GHC.Exts                      (SmallArray#, Array#, SmallMutableArray#, MutableArray#)

import qualified Data.Map                     as ML
import qualified Data.Set                     as S
import qualified Data.Map.Strict              as M
import qualified Language.Haskell.TH          as TH
import qualified Language.Haskell.TH.Datatype as TH
import qualified Language.Haskell.TH.Datatype.TyVarBndr as TH
import qualified Language.Haskell.TH.Ppr      as Ppr
import qualified Language.Haskell.TH.PprLib   as Ppr
import qualified Language.Haskell.TH.Syntax   as TH

newtype DeepStrictM a = DeepStrictM { runDeepStrictM :: ReaderT Context Q a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail, MonadReader Context)
  deriving (TH.Quote, TH.Quasi) via (ReaderT Context Q)

-- | Allow overriding various setting that determine what types we consider deep strict.
data Context = Context
  { contextSpine          :: !(S.Set TH.Type) -- ^ The types we are recursively checking. By the inductive hypothesis, we assume they are DeepStrict.
  , contextCache          :: !(IORef (M.Map TH.Type DeepStrictWithReason))
  , contextOverride       :: !(M.Map TH.Name (Maybe [ExpectedStrictness]))
  -- ^ Maps names of types to whether they can be deep strict and if they can which arguments need to be strict. 
  --
  -- In addition to user-provided types, a handful of internal GHC types are added as well.
  -- All of SmallArray#, SmallMutableArray#, MutableArray#, and Array# are added
  -- and are `Just [ExpectUnlifted]`.
  , contextRecursionDepth :: !Int -- ^ A recursion depth to avoid infinite loops.
  , contextArgs           :: ![TH.Type]
  }

-- | The default t'Context'.
emptyContext :: Q Context
emptyContext = do
  emptyCache <- TH.runIO $ newIORef M.empty
  pure $
    Context
      { contextSpine = S.empty
      , contextCache = emptyCache
      , contextOverride = M.empty
      , contextRecursionDepth = 1000
      , contextArgs = []
      }

-- | Structures that are present in base that need special treatment (they don't
-- have constructors we can inspect).
baseOverride :: M.Map TH.Name (Maybe [ExpectedStrictness])
baseOverride = M.fromList
  [ (''SmallArray#, Just [ExpectUnlifted])
  , (''SmallMutableArray#, Just [ExpectUnlifted])
  , (''MutableArray#, Just [ExpectUnlifted])
  , (''Array#, Just [ExpectUnlifted])
  ]

modifyArgsWith :: ([TH.Type] -> [TH.Type]) -> DeepStrictM a -> DeepStrictM a
modifyArgsWith f = local (\ctx -> ctx { contextArgs = f (contextArgs ctx) })

-- | A type is deep strict if and only if for each constructor:
--
--   - All of its fields are strict, ie, they have a @!@ if possible.
--   - The type of of each field is deep strict.
--
-- The Monoid instance allows us to gather up reasons why a type fails to be deep strict.
--
-- === Examples
--
-- @()@ is deep strict because its single constructor doesn't have any fields so it is vacuously deep strict.
--
-- 'Int', 'Char', etc are all deep strict because they are wrappers around unlifted types that cannot be lazy.
--
-- @Maybe Int@ is not deep strict.
-- It has a 'Nothing' constructor, which is fine.
-- But, the 'Just' constructor has a lazy field, which means it's not deep strict.
data DeepStrict reason =
    DeepStrict
  | NotDeepStrict !reason
  deriving (Eq, Ord, Show, TH.Lift, Functor)

type DeepStrictWithReason = DeepStrict [DeepStrictReason]

instance Semigroup reason => Semigroup (DeepStrict reason) where
  DeepStrict <> DeepStrict                       = DeepStrict
  NotDeepStrict reason <> DeepStrict             = NotDeepStrict reason
  DeepStrict <> NotDeepStrict reason             = NotDeepStrict reason
  NotDeepStrict reason1 <> NotDeepStrict reason2 = NotDeepStrict $ reason1 <> reason2

instance Semigroup reason => Monoid (DeepStrict reason) where
  mempty = DeepStrict

-- | Reasons why a type fails to be deep strict.
data DeepStrictReason =
    LazyType !TH.Type ![DeepStrictReason]
  -- ^ The type is lazy.
  | LazyConstructor !TH.Name ![DeepStrictReason]
  -- ^ The type has a lazy constructor.
  | FieldReason !FieldKey ![DeepStrictReason]
  -- ^ One of the fields of the constructor fails to be deep strict.
  | LazyField !FieldKey
  -- ^ One of the fields of the constructor is lazy, ie, doesn't have a @!@.
  | LazyNotUnlifted !(Either TH.Type TH.Name) ![DeepStrictReason]
  -- ^ A type we expected to be unlifted so a parent type could be strict, was not unlifted.
  | LazyOther !String
  deriving (Eq, Ord, Show, TH.Lift)

instance Ppr.Ppr reason => Ppr.Ppr (DeepStrict reason) where
  ppr DeepStrict             = Ppr.text "DeepStrict" Ppr.$+$ Ppr.text ""
  ppr (NotDeepStrict reason) = Ppr.text "NotDeepStrict" Ppr.$$ Ppr.ppr reason Ppr.$+$ Ppr.text ""

instance Ppr.Ppr DeepStrictReason where
  ppr (LazyType typ rest) = Ppr.hang (Ppr.ppr typ) 2 (Ppr.vcat (map Ppr.ppr rest))
  ppr (LazyConstructor name rest) = Ppr.hang (Ppr.text "con" Ppr.<+> Ppr.ppr name) 2 $ Ppr.vcat $ map Ppr.ppr rest
  ppr (FieldReason (Left ix) rest) = Ppr.hang (Ppr.text "field" Ppr.<+> Ppr.int ix) 2  $ Ppr.vcat $ map Ppr.ppr rest
  ppr (FieldReason (Right name) rest) = Ppr.hang (Ppr.ppr name) 2  $ Ppr.vcat $ map Ppr.ppr rest
  ppr (LazyField (Left ix)) = Ppr.text "field" Ppr.<+> Ppr.int ix Ppr.<+> Ppr.text "is lazy"
  ppr (LazyField (Right name)) = Ppr.text "field" Ppr.<+>  Ppr.ppr name Ppr.<+> Ppr.text "is lazy"
  ppr (LazyNotUnlifted name rest) = Ppr.hang (either Ppr.ppr Ppr.ppr name Ppr.<+> Ppr.text "isn't unlifted") 2 (Ppr.vcat (map Ppr.ppr rest))
  ppr (LazyOther txt) = Ppr.text txt

giveReasonContext :: ([DeepStrictReason] -> DeepStrictReason) -> DeepStrictWithReason  -> DeepStrictWithReason
giveReasonContext f =  fmap (pure . f)

prettyPanic :: (HasCallStack, Ppr.Ppr x, Show x) => String -> x -> a
prettyPanic context x = error $ context <> ": " <> Ppr.pprint x

data Levity = Lifted | Unlifted
  deriving (Eq, Ord, Show)

-- | Whether a type is used strictly by a data type.
-- We use these to annotate types with deep strictness overrides.
-- Types that have fields labelled as 'Language.Haskell.TH.DeepStrict.Strict' require those types to be deep strict.
-- Types that have fields labelled as 'Language.Haskell.TH.DeepStrict.Lazy' will never be deep strict, but this can be helpful for nicer messages.
data ExpectedStrictness
  = ExpectUnlifted -- ^ type param has to be unlifted for type to be deepstrict (Array#, SmallArray#, etc)
  | ExpectStrict -- ^ type param has to be deep strict for type to be strict (strict containers)
  | ExpectAnything -- ^ type is deep strict irrespective of the value of this type parameter, eg, if it is unused
  deriving (Eq, Ord, Show)

-- | A function/constructor is weak strict either iff it is strict and the argument isn't unlifted
-- So, it is like strictness but functions/constructors with unlifted/newtype args are WeakLazy
-- See: https://gitlab.haskell.org/ghc/ghc/-/issues/21380
data WeakStrictness = WeakStrict | WeakLazy
  deriving (Eq, Ord, Show)

data HasBang = HasBang | NoBang
  deriving (Eq, Ord, Show)

type FieldKey = Either Int TH.Name

type Env = ML.Map TH.Name TH.Type

withEnv :: [TH.Name] -> (Env -> DeepStrictM a) -> DeepStrictM a
withEnv argNames f = do
  args <- asks contextArgs
  let env = ML.fromList $ zip argNames args
  modifyArgsWith (drop (ML.size env)) (f env)


isDatatypeDeepStrict :: HasCallStack => TH.DatatypeInfo -> DeepStrictM DeepStrictWithReason
isDatatypeDeepStrict dt@(TH.DatatypeInfo {TH.datatypeInstTypes = instTypes}) =
  withEnv (mapMaybe getVariable instTypes) $ \env ->
    isDatatypeDeepStrict' dt { TH.datatypeCons = TH.applySubstitution env (TH.datatypeCons dt) }
  where
  getVariable :: TH.Type -> Maybe TH.Name
  getVariable (TH.SigT t _k) = getVariable t
  getVariable (TH.VarT v) = Just v
  getVariable _ = Nothing

  isDatatypeDeepStrict' :: HasCallStack => TH.DatatypeInfo -> DeepStrictM DeepStrictWithReason
  isDatatypeDeepStrict' updatedDt = do
    consDeepStrict <- traverse (\c -> isConDeepStrict c (TH.datatypeVariant updatedDt)) $ TH.datatypeCons updatedDt
    pure $ mconcat consDeepStrict

isConDeepStrict :: HasCallStack => TH.ConstructorInfo -> TH.DatatypeVariant -> DeepStrictM DeepStrictWithReason
isConDeepStrict (TH.ConstructorInfo { TH.constructorName = conName, TH.constructorFields = fieldTypes, TH.constructorVariant = conVar }) variant = do
  fieldBangs <-
    if isNewtype variant
    then pure $ repeat WeakStrict -- newtypes are strict
    else map decodeDecidedStrictness <$> TH.qReifyConStrictness conName
  fieldDeepStrict <- sequence $ zipWith3 isFieldDeepStrict fieldNames fieldBangs fieldTypes
  pure $ giveReasonContext (LazyConstructor conName) $ mconcat fieldDeepStrict
  where
  decodeDecidedStrictness :: TH.DecidedStrictness -> WeakStrictness
  decodeDecidedStrictness TH.DecidedStrict = WeakStrict
  decodeDecidedStrictness TH.DecidedUnpack = WeakStrict
  decodeDecidedStrictness TH.DecidedLazy   = WeakLazy

  -- | Figure out the field names for a constructor.
  -- We have names for records, we use indices for everything else.
  fieldNames :: [FieldKey]
  fieldNames = case conVar of
    (TH.RecordConstructor theFieldNames) -> map Right theFieldNames
    _ -> map Left [0..]

  isNewtype :: TH.DatatypeVariant -> Bool
  isNewtype TH.Newtype         = True
  isNewtype TH.NewtypeInstance = True
  isNewtype _                  = False

  isFieldDeepStrict :: HasCallStack => FieldKey -> WeakStrictness -> TH.Type -> DeepStrictM DeepStrictWithReason
  isFieldDeepStrict fieldName fieldWeakStrictness fieldType = do
    fieldTypeRecStrict <- isTypeDeepStrict fieldType
    fieldLevity <- reifyLevityType fieldType
    case (fieldWeakStrictness, fieldTypeRecStrict, fieldLevity) of
      (WeakStrict, DeepStrict, _) -> pure DeepStrict
      (WeakLazy, DeepStrict, Unlifted) -> pure DeepStrict
      (WeakLazy, strictness, Lifted) -> pure $ NotDeepStrict [LazyField fieldName] <> inField strictness
      (_, strictness, _) -> pure $ inField strictness
    where
    inField = giveReasonContext (FieldReason fieldName)

  reifyLevityType :: HasCallStack => TH.Type -> DeepStrictM Levity
  reifyLevityType = \case
    (TH.ConT name) -> classifyKindLevity <$> TH.qReifyType name
    (TH.AppT x _)  -> reifyLevityType x
    (TH.ListT{})   -> pure Lifted
    (TH.TupleT{})  -> pure Lifted
    (TH.ArrowT{})  -> pure Lifted
    (TH.UnboxedTupleT{}) -> pure Unlifted
    (TH.UnboxedSumT{}) -> pure Unlifted
    typ -> prettyPanic "unexpected type" typ
    where
    -- | Figure out the levity of a type from its kind.
    --   If it has type arguments the kind will have arrows, we want to know the final return type.
    --   Eg, for (x -> (y -> z)), we care about z
    classifyKindLevity :: TH.Kind -> Levity
    classifyKindLevity (TH.AppT _ x) = classifyKindLevity x
    classifyKindLevity TH.StarT      = Lifted
    classifyKindLevity _             = Unlifted

getCachedDeepStrict :: HasCallStack => TH.Type -> DeepStrictM (Maybe DeepStrictWithReason)
getCachedDeepStrict typ = do
  cacheRef <- asks contextCache
  cache <- TH.qRunIO $ readIORef cacheRef
  pure $ M.lookup typ cache

putCachedDeepStrict :: HasCallStack => TH.Type -> DeepStrictWithReason  -> DeepStrictM ()
putCachedDeepStrict typ val = do
  cacheRef <- asks contextCache
  TH.qRunIO . modifyIORef' cacheRef $ M.insert typ ([LazyOther $ Ppr.pprint typ <> " is lazy see above"] <$ val)

isTypeDeepStrict :: HasCallStack => TH.Type -> DeepStrictM DeepStrictWithReason
isTypeDeepStrict typ = do
  ctxt <- ask
  cachedVal <- getCachedDeepStrict typ
  when (contextRecursionDepth ctxt <= 0) . fail $ "Recursion depth reached. Try adding an override for this type: " <> take 1000 (show typ)
  case (cachedVal, S.member typ $ contextSpine ctxt) of
    (Just val, _) -> pure val
    (_, True) -> pure DeepStrict -- by inductive hypothesis
    _ ->
      local (const
        ctxt {contextSpine = S.insert typ (contextSpine ctxt), contextRecursionDepth = contextRecursionDepth ctxt - 1}) $ do
          ret <- inType <$> isTypeDeepStrict' typ
          putCachedDeepStrict typ ret
          pure ret
  where
    inType = giveReasonContext (LazyType typ)

isTypeDeepStrict' :: HasCallStack => TH.Type -> DeepStrictM DeepStrictWithReason
isTypeDeepStrict' (TH.ConT typeName) = isNameDeepStrict typeName
isTypeDeepStrict' (TH.AppT func arg) = modifyArgsWith (arg:) $ isTypeDeepStrict' func
isTypeDeepStrict' (TH.TupleT 0)      = pure DeepStrict -- () is DeepStrict
isTypeDeepStrict' (TH.TupleT n)      = isNameDeepStrict (TH.tupleTypeName n)
isTypeDeepStrict' (TH.ArrowT{})      = pure $ NotDeepStrict [LazyOther "Functions are lazy"]
isTypeDeepStrict' (TH.ListT{})       = isNameDeepStrict ''[]
isTypeDeepStrict' (TH.UnboxedTupleT arity) = isNameDeepStrict (TH.unboxedTupleTypeName arity)
isTypeDeepStrict' (TH.UnboxedSumT arity) = isNameDeepStrict (TH.unboxedSumTypeName arity)
isTypeDeepStrict' typ                    = prettyPanic "Unexpected type" typ

-- | figure out whether a newtype/data family is deep strict
isDataFamilyDeepStrict
  :: (p1 -> TH.Name -> [TH.TyVarBndr TH.BndrVis] -> p2 -> p3 -> p4 -> TH.Dec)
  -> TH.Name
  -> p1
  -> Maybe [TH.TyVarBndr ()]
  -> TH.Type
  -> p2
  -> p3
  -> p4
  -> DeepStrictM DeepStrictWithReason
isDataFamilyDeepStrict dConstr typeName cxt mTyVarBndrs typ kind con deriv =  do
  let tyVarBndrs = fromMaybe [] mTyVarBndrs
  let
    mkRequiredVis (TH.PlainTV x _) = TH.plainTVFlag x TH.BndrReq
    mkRequiredVis (TH.KindedTV x _ k) = TH.kindedTVFlag x TH.BndrReq k
  args <- asks contextArgs
  let appliedArgs = foldl' TH.AppT (TH.ConT typeName) args
  let d = dConstr cxt (TH.mkName $ TH.pprint appliedArgs)  (map mkRequiredVis tyVarBndrs) kind con deriv
  datatypeInfo <- DeepStrictM $ lift $ TH.normalizeDec d
  -- figure out the mapping from the input to the free variables in the family instance
  unified <- DeepStrictM . lift $ TH.unifyTypes [appliedArgs, typ]
  let tyVarNotFound = error "unmatched type variable in a data family definition"
  -- now our args are those free variables with the mapping
  let args' = map (fromMaybe tyVarNotFound . flip M.lookup unified . TH.tvName) tyVarBndrs
  modifyArgsWith (const args') $ isDatatypeDeepStrict datatypeInfo

-- | Is this type constructor applied to these arguments deep strict
isNameDeepStrict :: HasCallStack => TH.Name -> DeepStrictM DeepStrictWithReason
isNameDeepStrict typeName = do
  ctxt <- ask
  case M.lookup typeName $ contextOverride ctxt of
    Nothing -> do
      info <- DeepStrictM $ lift $ TH.reify typeName
      case info of
        -- th-abstraction can't handle type synonyms.
        -- let's treat a type synonym as just the RHS
        TH.TyConI (TH.TySynD _name tyvarbndrs rhs) -> do
          withEnv (map TH.tvName tyvarbndrs) $ \env ->
            isTypeDeepStrict (TH.applySubstitution env rhs)
        -- handle type/data families
        TH.FamilyI{} -> do
          instances <- DeepStrictM $ lift $ TH.reifyInstances typeName (contextArgs ctxt)
          case instances of
            -- a type synonym instance is handled like a type synonym:
            -- just treat it as the RHS.
            (TH.TySynInstD (TH.TySynEqn _ lhs rhs)):_ -> do
              withEnv (TH.freeVariables lhs) $ \env ->
                isTypeDeepStrict (TH.applySubstitution env rhs)
            -- let's construct a dummy datatype decleration
            (TH.DataInstD cxt mTyVarBndrs typ kind con deriv):_ -> do
              isDataFamilyDeepStrict TH.DataD typeName cxt mTyVarBndrs typ kind con deriv
            (TH.NewtypeInstD cxt mTyVarBndrs typ kind con deriv):_ -> do
              isDataFamilyDeepStrict TH.NewtypeD typeName cxt mTyVarBndrs typ kind con deriv
            _ -> error "Unsupported/ambiguous data/type family"
        _ -> do
          datatypeInfo <- DeepStrictM $ lift $ TH.normalizeInfo info
          isDatatypeDeepStrict datatypeInfo
    Just Nothing -> pure $ NotDeepStrict [LazyOther "This type is marked as lazy"]
    Just (Just strictnessReqs) ->
      fmap mconcat . for (zip strictnessReqs (contextArgs ctxt)) $ \case
        (ExpectAnything, _)     -> pure DeepStrict
        (ExpectStrict, typ) -> modifyArgsWith (const []) $ isTypeDeepStrict typ
        (ExpectUnlifted, typ) -> do
          levity <- reifyLevityType typ
          case levity of
            Lifted -> pure $ NotDeepStrict [LazyNotUnlifted (Left typ) []]
            Unlifted -> modifyArgsWith (const []) $ isTypeDeepStrict typ

-- | Determine if a type is deep strict
-- Invariant: The type doesn't contain any free variables, eg, @Maybe a@ will fail.
isDeepStrict :: TH.Type -> Q DeepStrictWithReason
isDeepStrict typ = do
  emptyC <- emptyContext
  isDeepStrictWith emptyC typ

isDeepStrictWith :: Context -> TH.Type -> Q DeepStrictWithReason
isDeepStrictWith context typ = do
  typRes <- TH.resolveTypeSynonyms typ
  runReaderT (runDeepStrictM $ isTypeDeepStrict typRes) context { contextOverride = contextOverride context <> baseOverride }


-- | Assert that a type is deep strict.
-- If the type isn't deep strict then this will produce an error with the reasons why.
assertDeepStrict :: TH.Type -> Q [TH.Dec]
assertDeepStrict typ = do
  emptyC <- emptyContext
  assertDeepStrictWith emptyC typ

data DeepStrictAssertionFailed = DeepStrictAssertionFailed TH.Type [DeepStrictReason]

instance Ppr.Ppr DeepStrictAssertionFailed where
  ppr (DeepStrictAssertionFailed typ reason) =
   Ppr.ppr typ Ppr.$+$ Ppr.text "is not Deep Strict, because: "
   Ppr.$$ Ppr.ppr reason

assertDeepStrictWith :: Context -> TH.Type -> Q [TH.Dec]
assertDeepStrictWith context typ = do
  result <- isDeepStrictWith context typ
  case result of
    DeepStrict -> pure []
    NotDeepStrict reason ->
      fail $ Ppr.pprint $ DeepStrictAssertionFailed typ reason
