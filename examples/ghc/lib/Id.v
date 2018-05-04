(* Default settings (from HsToCoq.Coq.Preamble) *)

Generalizable All Variables.

Unset Implicit Arguments.
Set Maximal Implicit Insertion.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Require Coq.Program.Tactics.
Require Coq.Program.Wf.

(* Preamble *)

Require Import Core.


(* Converted imports: *)

Require BasicTypes.
Require Core.
Require Datatypes.
Require FastString.
Require GHC.Base.
Require GHC.Enum.
Require GHC.List.
Require GHC.Num.
Require GHC.Prim.
Require Maybes.
Require Module.
Require Name.
Require OccName.
Require Panic.
Require SrcLoc.
Require UniqSupply.
Require Unique.
Require Util.
Import GHC.Base.Notations.
Import GHC.Num.Notations.

(* No type declarations to convert. *)
(* Midamble *)

Parameter lookupDataCon : Core.DataConId -> Core.DataCon.
Parameter lookupClass   : Core.ClassId -> Core.Class.

(* Make this default so that we can reason about either case. *)
Import GHC.Err.
Definition isStateHackType : unit -> bool := GHC.Err.default.

(* The real definition looks like this, but we don't have the type information
   around:
  fun ty =>
    if DynFlags.hasNoStateHack DynFlags.unsafeGlobalDynFlags : bool then false else
    match Type.tyConAppTyCon_maybe ty with
    | Some tycon => tycon GHC.Base.== TysPrim.statePrimTyCon
    | _ => false
    end. *)



(* Converted value declarations: *)

Definition asJoinId : Core.Var -> BasicTypes.JoinArity -> Core.JoinId :=
  fun id arity =>
    let is_vanilla_or_join :=
      fun id =>
        match Core.idDetails id with
        | Core.VanillaId => true
        | Core.Mk_JoinId _ => true
        | _ => false
        end in
    Panic.warnPprTrace (negb (Core.isLocalId id)) (GHC.Base.hs_string__
                        "ghc/compiler/basicTypes/Id.hs") #590 (GHC.Base.mappend (Datatypes.id
                                                                                 (GHC.Base.hs_string__
                                                                                  "global id being marked as join var:"))
                                                                                (Panic.noString id)) (Panic.warnPprTrace
                                                                                                      (negb
                                                                                                       (is_vanilla_or_join
                                                                                                        id))
                                                                                                      (GHC.Base.hs_string__
                                                                                                       "ghc/compiler/basicTypes/Id.hs")
                                                                                                      #592
                                                                                                      (GHC.Base.mappend
                                                                                                       (Panic.noString
                                                                                                        id)
                                                                                                       Panic.someSDoc)
                                                                                                      (Core.setIdDetails
                                                                                                       id
                                                                                                       (Core.Mk_JoinId
                                                                                                        arity))).

Definition hasNoBinding : Core.Var -> bool :=
  fun id =>
    match Core.idDetails id with
    | Core.PrimOpId _ => true
    | Core.FCallId _ => true
    | Core.DataConWorkId dc =>
        orb (Core.isUnboxedTupleCon dc) (Core.isUnboxedSumCon dc)
    | _ => false
    end.

Definition idArity : Core.Var -> nat :=
  fun id => Core.arityInfo ((@Core.idInfo tt id)).

Definition idCafInfo : Core.Var -> Core.CafInfo :=
  fun id => Core.cafInfo ((@Core.idInfo tt id)).

Definition idCallArity : Core.Var -> nat :=
  fun id => Core.callArityInfo ((@Core.idInfo tt id)).

Definition idDemandInfo : Core.Var -> Core.Demand :=
  fun id => Core.demandInfo ((@Core.idInfo tt id)).

Definition idInlinePragma : Core.Var -> BasicTypes.InlinePragma :=
  fun id => Core.inlinePragInfo ((@Core.idInfo tt id)).

Definition idRuleMatchInfo : Core.Var -> BasicTypes.RuleMatchInfo :=
  fun id => BasicTypes.inlinePragmaRuleMatchInfo (idInlinePragma id).

Definition idInlineActivation : Core.Var -> BasicTypes.Activation :=
  fun id => BasicTypes.inlinePragmaActivation (idInlinePragma id).

Definition idName : Core.Var -> Name.Name :=
  Core.varName.

Definition localiseId : Core.Var -> Core.Var :=
  fun id =>
    let name := idName id in
    if andb (Core.isLocalId id) (Name.isInternalName name) : bool then id else
    Core.mkLocalVar (Core.idDetails id) (Name.localiseName name) (tt) ((@Core.idInfo
                                                                        tt id)).

Definition idIsFrom : Module.Module -> Core.Var -> bool :=
  fun mod_ id => Name.nameIsLocalOrFrom mod_ (idName id).

Definition idOccInfo : Core.Var -> BasicTypes.OccInfo :=
  fun id => Core.occInfo ((@Core.idInfo tt id)).

Definition isDeadBinder : Core.Var -> bool :=
  fun bndr =>
    if Core.isId bndr : bool then BasicTypes.isDeadOcc (idOccInfo bndr) else
    false.

Definition idOneShotInfo : Core.Var -> BasicTypes.OneShotInfo :=
  fun id => Core.oneShotInfo ((@Core.idInfo tt id)).

Definition idSpecialisation : Core.Var -> Core.RuleInfo :=
  fun id => Core.ruleInfo ((@Core.idInfo tt id)).

Definition idHasRules : Core.Var -> bool :=
  fun id => negb (Core.isEmptyRuleInfo (idSpecialisation id)).

Definition idStrictness : Core.Var -> Core.StrictSig :=
  fun id => Core.strictnessInfo ((@Core.idInfo tt id)).

Definition isBottomingId : Core.Var -> bool :=
  fun v =>
    if Core.isId v : bool then Core.isBottomingSig (idStrictness v) else
    false.

Definition idType : Core.Var -> unit :=
  Core.varType.

Definition idUnique : Core.Var -> Unique.Unique :=
  Core.varUnique.

Definition isClassOpId_maybe : Core.Var -> option Core.Class :=
  fun id =>
    match Core.idDetails id with
    | Core.ClassOpId cls => Some cls
    | _other => None
    end.

Definition isDFunId : Core.Var -> bool :=
  fun id =>
    match Core.idDetails id with
    | Core.Mk_DFunId _ => true
    | _ => false
    end.

Definition isDataConId_maybe : Core.Var -> option Core.DataCon :=
  fun id =>
    match Core.idDetails id with
    | Core.DataConWorkId con => Some con
    | Core.DataConWrapId con => Some con
    | _ => None
    end.

Definition idDataCon : Core.Var -> Core.DataCon :=
  fun id =>
    Maybes.orElse (isDataConId_maybe id) (Panic.panicStr (GHC.Base.hs_string__
                                                          "idDataCon") (Panic.noString id)).

Definition isDataConRecordSelector : Core.Var -> bool :=
  fun id =>
    match Core.idDetails id with
    | Core.RecSelId (Core.RecSelData _) _ => true
    | _ => false
    end.

Definition isDataConWorkId : Core.Var -> bool :=
  fun id =>
    match Core.idDetails id with
    | Core.DataConWorkId _ => true
    | _ => false
    end.

Definition isConLikeId : Core.Var -> bool :=
  fun id => orb (isDataConWorkId id) (BasicTypes.isConLike (idRuleMatchInfo id)).

Definition isDataConWorkId_maybe : Core.Var -> option Core.DataCon :=
  fun id =>
    match Core.idDetails id with
    | Core.DataConWorkId con => Some con
    | _ => None
    end.

Definition isFCallId : Core.Var -> bool :=
  fun id =>
    match Core.idDetails id with
    | Core.FCallId _ => true
    | _ => false
    end.

Definition isFCallId_maybe : Core.Var -> option unit :=
  fun id =>
    match Core.idDetails id with
    | Core.FCallId call => Some call
    | _ => None
    end.

Definition isImplicitId : Core.Var -> bool :=
  fun id =>
    match Core.idDetails id with
    | Core.FCallId _ => true
    | Core.ClassOpId _ => true
    | Core.PrimOpId _ => true
    | Core.DataConWorkId _ => true
    | Core.DataConWrapId _ => true
    | _ => false
    end.

Definition isJoinId : Core.Var -> bool :=
  fun id =>
    if Core.isId id : bool
    then match Core.idDetails id with
         | Core.Mk_JoinId _ => true
         | _ => false
         end else
    false.

Definition isExitJoinId : Core.Var -> bool :=
  fun id =>
    andb (isJoinId id) (andb (BasicTypes.isOneOcc (idOccInfo id))
                             (BasicTypes.occ_in_lam (idOccInfo id))).

Definition isJoinId_maybe : Core.Var -> option BasicTypes.JoinArity :=
  fun id =>
    if Core.isId id : bool
    then match Core.idDetails id with
         | Core.Mk_JoinId arity => Some arity
         | _ => None
         end else
    None.

Definition idJoinArity : Core.JoinId -> BasicTypes.JoinArity :=
  fun id =>
    Maybes.orElse (isJoinId_maybe id) (Panic.panicStr (GHC.Base.hs_string__
                                                       "idJoinArity") (Panic.noString id)).

Definition isNaughtyRecordSelector : Core.Var -> bool :=
  fun id =>
    match Core.idDetails id with
    | Core.RecSelId _ n => n
    | _ => false
    end.

Definition isPatSynRecordSelector : Core.Var -> bool :=
  fun id =>
    match Core.idDetails id with
    | Core.RecSelId (Core.RecSelPatSyn _) _ => true
    | _ => false
    end.

Definition isPrimOpId : Core.Var -> bool :=
  fun id =>
    match Core.idDetails id with
    | Core.PrimOpId _ => true
    | _ => false
    end.

Definition isPrimOpId_maybe : Core.Var -> option unit :=
  fun id =>
    match Core.idDetails id with
    | Core.PrimOpId op => Some op
    | _ => None
    end.

Definition isRecordSelector : Core.Var -> bool :=
  fun id =>
    match Core.idDetails id with
    | Core.RecSelId _ _ => true
    | _ => false
    end.

Definition lazySetIdInfo : Core.Var -> Core.IdInfo -> Core.Var :=
  Core.lazySetIdInfo.

Definition maybeModifyIdInfo : option Core.IdInfo -> Core.Var -> Core.Var :=
  fun arg_0__ arg_1__ =>
    match arg_0__, arg_1__ with
    | Some new_info, id => lazySetIdInfo id new_info
    | None, id => id
    end.

Definition zapInfo
   : (Core.IdInfo -> option Core.IdInfo) -> Core.Var -> Core.Var :=
  fun zapper id => maybeModifyIdInfo (zapper ((@Core.idInfo tt id))) id.

Definition zapIdDemandInfo : Core.Var -> Core.Var :=
  zapInfo Core.zapDemandInfo.

Definition zapIdTailCallInfo : Core.Var -> Core.Var :=
  zapInfo Core.zapTailCallInfo.

Definition zapJoinId : Core.Var -> Core.Var :=
  fun jid =>
    if isJoinId jid : bool
    then zapIdTailCallInfo (Core.setIdDetails jid Core.VanillaId) else
    jid.

Definition asJoinId_maybe
   : Core.Var -> option BasicTypes.JoinArity -> Core.Var :=
  fun arg_0__ arg_1__ =>
    match arg_0__, arg_1__ with
    | id, Some arity => asJoinId id arity
    | id, None => zapJoinId id
    end.

Definition zapIdUsageInfo : Core.Var -> Core.Var :=
  zapInfo Core.zapUsageInfo.

Definition zapIdUsedOnceInfo : Core.Var -> Core.Var :=
  zapInfo Core.zapUsedOnceInfo.

Definition zapLamIdInfo : Core.Var -> Core.Var :=
  zapInfo Core.zapLamInfo.

Definition setIdInfo : Core.Var -> Core.IdInfo -> Core.Var :=
  fun id info => GHC.Prim.seq info (lazySetIdInfo id info).

Definition modifyIdInfo
   : (Core.IdInfo -> Core.IdInfo) -> Core.Var -> Core.Var :=
  fun fn id => setIdInfo id (fn ((@Core.idInfo tt id))).

Definition modifyInlinePragma
   : Core.Var ->
     (BasicTypes.InlinePragma -> BasicTypes.InlinePragma) -> Core.Var :=
  fun id fn =>
    modifyIdInfo (fun info =>
                    Core.setInlinePragInfo info (fn (Core.inlinePragInfo info))) id.

Definition setInlineActivation
   : Core.Var -> BasicTypes.Activation -> Core.Var :=
  fun id act =>
    modifyInlinePragma id (fun prag =>
                             BasicTypes.setInlinePragmaActivation prag act).

Definition setIdArity : Core.Var -> nat -> Core.Var :=
  fun id arity =>
    modifyIdInfo (fun arg_0__ => Core.setArityInfo arg_0__ arity) id.

Definition setIdCafInfo : Core.Var -> Core.CafInfo -> Core.Var :=
  fun id caf_info =>
    modifyIdInfo (fun arg_0__ => Core.setCafInfo arg_0__ caf_info) id.

Definition setIdCallArity : Core.Var -> nat -> Core.Var :=
  fun id arity =>
    modifyIdInfo (fun arg_0__ => Core.setCallArityInfo arg_0__ arity) id.

Definition setIdDemandInfo : Core.Var -> Core.Demand -> Core.Var :=
  fun id dmd => modifyIdInfo (fun arg_0__ => Core.setDemandInfo arg_0__ dmd) id.

Definition setIdOccInfo : Core.Var -> BasicTypes.OccInfo -> Core.Var :=
  fun id occ_info =>
    modifyIdInfo (fun arg_0__ => Core.setOccInfo arg_0__ occ_info) id.

Definition zapIdOccInfo : Core.Var -> Core.Var :=
  fun b => setIdOccInfo b BasicTypes.noOccInfo.

Definition setIdOneShotInfo : Core.Var -> BasicTypes.OneShotInfo -> Core.Var :=
  fun id one_shot =>
    modifyIdInfo (fun arg_0__ => Core.setOneShotInfo arg_0__ one_shot) id.

Definition updOneShotInfo : Core.Var -> BasicTypes.OneShotInfo -> Core.Var :=
  fun id one_shot =>
    let do_upd :=
      match pair (idOneShotInfo id) one_shot with
      | pair BasicTypes.NoOneShotInfo _ => true
      | pair BasicTypes.OneShotLam _ => false
      end in
    if do_upd : bool then setIdOneShotInfo id one_shot else
    id.

Definition setIdSpecialisation : Core.Var -> Core.RuleInfo -> Core.Var :=
  fun id spec_info =>
    modifyIdInfo (fun arg_0__ => Core.setRuleInfo arg_0__ spec_info) id.

Definition setIdStrictness : Core.Var -> Core.StrictSig -> Core.Var :=
  fun id sig =>
    modifyIdInfo (fun arg_0__ => Core.setStrictnessInfo arg_0__ sig) id.

Definition setInlinePragma : Core.Var -> BasicTypes.InlinePragma -> Core.Var :=
  fun id prag =>
    modifyIdInfo (fun arg_0__ => Core.setInlinePragInfo arg_0__ prag) id.

Definition setOneShotLambda : Core.Var -> Core.Var :=
  fun id =>
    modifyIdInfo (fun arg_0__ => Core.setOneShotInfo arg_0__ BasicTypes.OneShotLam)
    id.

Definition transferPolyIdInfo
   : Core.Var -> list Core.Var -> Core.Var -> Core.Var :=
  fun old_id abstract_wrt new_id =>
    let old_info := (@Core.idInfo tt old_id) in
    let old_arity := Core.arityInfo old_info in
    let old_inline_prag := Core.inlinePragInfo old_info in
    let old_occ_info := Core.occInfo old_info in
    let new_occ_info := BasicTypes.zapOccTailCallInfo old_occ_info in
    let old_strictness := Core.strictnessInfo old_info in
    let arity_increase := Util.count Core.isId abstract_wrt in
    let new_arity := old_arity GHC.Num.+ arity_increase in
    let new_strictness :=
      Core.increaseStrictSigArity arity_increase old_strictness in
    let transfer :=
      fun new_info =>
        Core.setStrictnessInfo (Core.setOccInfo (Core.setInlinePragInfo
                                                 (Core.setArityInfo new_info new_arity) old_inline_prag) new_occ_info)
                               new_strictness in
    modifyIdInfo transfer new_id.

Definition zapIdStrictness : Core.Var -> Core.Var :=
  fun id =>
    modifyIdInfo (fun arg_0__ => Core.setStrictnessInfo arg_0__ Core.nopSig) id.

Definition clearOneShotLambda : Core.Var -> Core.Var :=
  fun id =>
    modifyIdInfo (fun arg_0__ =>
                    Core.setOneShotInfo arg_0__ BasicTypes.NoOneShotInfo) id.

Definition mkExportedLocalId
   : Core.IdDetails -> Name.Name -> unit -> Core.Var :=
  fun details name ty =>
    Core.mkExportedLocalVar details name ty Core.vanillaIdInfo.

Definition mkExportedVanillaId : Name.Name -> unit -> Core.Var :=
  fun name ty =>
    Core.mkExportedLocalVar Core.VanillaId name ty Core.vanillaIdInfo.

Definition mkGlobalId
   : Core.IdDetails -> Name.Name -> unit -> Core.IdInfo -> Core.Var :=
  Core.mkGlobalVar.

Definition mkVanillaGlobalWithInfo
   : Name.Name -> unit -> Core.IdInfo -> Core.Var :=
  mkGlobalId Core.VanillaId.

Definition mkVanillaGlobal : Name.Name -> unit -> Core.Var :=
  fun name ty => mkVanillaGlobalWithInfo name ty Core.vanillaIdInfo.

Definition mkLocalIdOrCoVarWithInfo
   : Name.Name -> unit -> Core.IdInfo -> Core.Var :=
  fun name ty info =>
    let details := Core.VanillaId in Core.mkLocalVar details name ty info.

Definition mkLocalIdWithInfo : Name.Name -> unit -> Core.IdInfo -> Core.Var :=
  fun name ty info => Core.mkLocalVar Core.VanillaId name ty info.

Definition mkLocalId : Name.Name -> unit -> Core.Var :=
  fun name ty => mkLocalIdWithInfo name ty Core.vanillaIdInfo.

Definition mkLocalIdOrCoVar : Name.Name -> unit -> Core.Var :=
  fun name ty => mkLocalId name ty.

Definition mkSysLocalOrCoVar
   : FastString.FastString -> Unique.Unique -> unit -> Core.Var :=
  fun fs uniq ty => mkLocalIdOrCoVar (Name.mkSystemVarName uniq fs) ty.

Definition mkSysLocalOrCoVarM {m} `{UniqSupply.MonadUnique m}
   : FastString.FastString -> unit -> m Core.Var :=
  fun fs ty =>
    UniqSupply.getUniqueM GHC.Base.>>=
    (fun uniq => GHC.Base.return_ (mkSysLocalOrCoVar fs uniq ty)).

Definition mkTemplateLocal : nat -> unit -> Core.Var :=
  fun i ty =>
    mkSysLocalOrCoVar (FastString.fsLit (GHC.Base.hs_string__ "v"))
    (Unique.mkBuiltinUnique i) ty.

Definition mkTemplateLocalsNum : nat -> list unit -> list Core.Var :=
  fun n tys => GHC.List.zipWith mkTemplateLocal (GHC.Enum.enumFrom n) tys.

Definition mkTemplateLocals : list unit -> list Core.Var :=
  mkTemplateLocalsNum #1.

Definition mkUserLocalOrCoVar
   : OccName.OccName -> Unique.Unique -> unit -> SrcLoc.SrcSpan -> Core.Var :=
  fun occ uniq ty loc => mkLocalIdOrCoVar (Name.mkInternalName uniq occ loc) ty.

Definition mkWorkerId : Unique.Unique -> Core.Var -> unit -> Core.Var :=
  fun uniq unwrkr ty =>
    mkLocalIdOrCoVar (Name.mkDerivedInternalName OccName.mkWorkerOcc uniq
                      (Name.getName unwrkr)) ty.

Definition mkSysLocal
   : FastString.FastString -> Unique.Unique -> unit -> Core.Var :=
  fun fs uniq ty => mkLocalId (Name.mkSystemVarName uniq fs) ty.

Definition mkSysLocalM {m} `{UniqSupply.MonadUnique m}
   : FastString.FastString -> unit -> m Core.Var :=
  fun fs ty =>
    UniqSupply.getUniqueM GHC.Base.>>=
    (fun uniq => GHC.Base.return_ (mkSysLocal fs uniq ty)).

Definition mkUserLocal
   : OccName.OccName -> Unique.Unique -> unit -> SrcLoc.SrcSpan -> Core.Var :=
  fun occ uniq ty loc => mkLocalId (Name.mkInternalName uniq occ loc) ty.

Definition recordSelectorTyCon : Core.Var -> Core.RecSelParent :=
  fun id =>
    match Core.idDetails id with
    | Core.RecSelId parent _ => parent
    | _ => Panic.panic (GHC.Base.hs_string__ "recordSelectorTyCon")
    end.

Definition setIdExported : Core.Var -> Core.Var :=
  Core.setIdExported.

Definition setIdName : Core.Var -> Name.Name -> Core.Var :=
  Core.setVarName.

Definition setIdNotExported : Core.Var -> Core.Var :=
  Core.setIdNotExported.

Definition setIdUnique : Core.Var -> Unique.Unique -> Core.Var :=
  Core.setVarUnique.

Definition stateHackOneShot : BasicTypes.OneShotInfo :=
  BasicTypes.OneShotLam.

Definition typeOneShot : unit -> BasicTypes.OneShotInfo :=
  fun ty =>
    if isStateHackType ty : bool then stateHackOneShot else
    BasicTypes.NoOneShotInfo.

Definition idStateHackOneShotInfo : Core.Var -> BasicTypes.OneShotInfo :=
  fun id =>
    if isStateHackType (tt) : bool then stateHackOneShot else
    idOneShotInfo id.

Definition isOneShotBndr : Core.Var -> bool :=
  fun var =>
    if Core.isTyVar var : bool then true else
    match idStateHackOneShotInfo var with
    | BasicTypes.OneShotLam => true
    | _ => false
    end.

Definition isProbablyOneShotLambda : Core.Var -> bool :=
  fun id =>
    match idStateHackOneShotInfo id with
    | BasicTypes.OneShotLam => true
    | BasicTypes.NoOneShotInfo => false
    end.

(* External variables:
     None Some andb bool false isStateHackType list nat negb option orb pair true tt
     unit BasicTypes.Activation BasicTypes.InlinePragma BasicTypes.JoinArity
     BasicTypes.NoOneShotInfo BasicTypes.OccInfo BasicTypes.OneShotInfo
     BasicTypes.OneShotLam BasicTypes.RuleMatchInfo BasicTypes.inlinePragmaActivation
     BasicTypes.inlinePragmaRuleMatchInfo BasicTypes.isConLike BasicTypes.isDeadOcc
     BasicTypes.isOneOcc BasicTypes.noOccInfo BasicTypes.occ_in_lam
     BasicTypes.setInlinePragmaActivation BasicTypes.zapOccTailCallInfo Core.CafInfo
     Core.Class Core.ClassOpId Core.DataCon Core.DataConWorkId Core.DataConWrapId
     Core.Demand Core.FCallId Core.IdDetails Core.IdInfo Core.JoinId Core.Mk_DFunId
     Core.Mk_JoinId Core.PrimOpId Core.RecSelData Core.RecSelId Core.RecSelParent
     Core.RecSelPatSyn Core.RuleInfo Core.StrictSig Core.VanillaId Core.Var
     Core.arityInfo Core.cafInfo Core.callArityInfo Core.demandInfo Core.idDetails
     Core.idInfo Core.increaseStrictSigArity Core.inlinePragInfo Core.isBottomingSig
     Core.isEmptyRuleInfo Core.isId Core.isLocalId Core.isTyVar Core.isUnboxedSumCon
     Core.isUnboxedTupleCon Core.lazySetIdInfo Core.mkExportedLocalVar
     Core.mkGlobalVar Core.mkLocalVar Core.nopSig Core.occInfo Core.oneShotInfo
     Core.ruleInfo Core.setArityInfo Core.setCafInfo Core.setCallArityInfo
     Core.setDemandInfo Core.setIdDetails Core.setIdExported Core.setIdNotExported
     Core.setInlinePragInfo Core.setOccInfo Core.setOneShotInfo Core.setRuleInfo
     Core.setStrictnessInfo Core.setVarName Core.setVarUnique Core.strictnessInfo
     Core.vanillaIdInfo Core.varName Core.varType Core.varUnique Core.zapDemandInfo
     Core.zapLamInfo Core.zapTailCallInfo Core.zapUsageInfo Core.zapUsedOnceInfo
     Datatypes.id FastString.FastString FastString.fsLit GHC.Base.mappend
     GHC.Base.op_zgzgze__ GHC.Base.return_ GHC.Enum.enumFrom GHC.List.zipWith
     GHC.Num.fromInteger GHC.Num.op_zp__ GHC.Prim.seq Maybes.orElse Module.Module
     Name.Name Name.getName Name.isInternalName Name.localiseName
     Name.mkDerivedInternalName Name.mkInternalName Name.mkSystemVarName
     Name.nameIsLocalOrFrom OccName.OccName OccName.mkWorkerOcc Panic.noString
     Panic.panic Panic.panicStr Panic.someSDoc Panic.warnPprTrace SrcLoc.SrcSpan
     UniqSupply.MonadUnique UniqSupply.getUniqueM Unique.Unique
     Unique.mkBuiltinUnique Util.count
*)
