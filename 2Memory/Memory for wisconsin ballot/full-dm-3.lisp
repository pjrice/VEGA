; includes the names of all candidates listed on the ballot


(add-dm


;****************************************
;****************************************

;Candidate Chunks

;****************************************
;****************************************

(GordonBearce ISA Candidate name "GordonBearce" party "Rep" race "PresidentoftheUnitedStates")
(VernonStanleyAlbury ISA Candidate name "VernonStanleyAlbury" party "Dem" race "PresidentoftheUnitedStates")
(JanetteFroman ISA Candidate name "JanetteFroman" party "Lib" race "PresidentoftheUnitedStates")
;;
(CecileCadieux ISA Candidate name "CecileCadieux" party "Rep" race "UnitedStatesSenator")
(FernBrzezinski ISA Candidate name "FernBrzezinski" party "Dem" race "UnitedStatesSenator")
(CoreyDery ISA Candidate name "CoreyDery" party "Ind" race "UnitedStatesSenator")
;;
(PedroBrouse ISA Candidate name "PedroBrouse" party "Rep" race "UnitedStatesRepresentativeDistrict7")
(RobertMettler ISA Candidate name "RobertMettler" party "Dem" race "UnitedStatesRepresentativeDistrict7")
;;
;;wisconsin
(JimDoyle ISA Candidate name "JimDoyle" party "DEM" race "GovernorLieutenantGovernor")
(ScottMcCallum ISA Candidate name "ScottMcCallum" party "REP" race "GovernorLieutenantGovernor")
(JimYoung ISA Candidate name "JimYoung" party "WIS" race "GovernorLieutenantGovernor")
(EdThompson ISA Candidate name "EdThompson" party "LIB" race "GovernorLieutenantGovernor")
(AlanEisenberg ISA Candidate name "AlanDEisenberg" party "REF" race "GovernorLieutenantGovernor")
(TyBollerud ISA Candidate name "TyABollerud" party "IND" race "GovernorLieutenantGovernor")
(MikeMangan ISA Candidate name "MikeMangan" party "GUE" race "GovernorLieutenantGovernor")
(AnebJahRasta ISA Candidate name "AnebJahRasta" party "RAS" race "GovernorLieutenantGovernor")
;;
;;
(TimSpeight ISA Candidate name "TimSpeight" party "Rep" race "AttorneyGeneral")
(RickOrgan ISA Candidate name "RickOrgan" party "Dem" race "AttorneyGeneral")
;;
(ThereseGustin ISA Candidate name "ThereseGustin" party "Ind" race "ComptrollerofPublicAccounts")
(GregConverse ISA Candidate name "GregConverse" party "Dem" race "ComptrollerofPublicAccounts")

;;
(EliseEllzey ISA Candidate name "EliseEllzey" party "Dem" race "CommissionerofGeneralLandOffice")
(SamSaddler ISA Candidate name "SamSaddler" party "Rep" race "CommissionerofGeneralLandOffice")

;;
(PollyRylander ISA Candidate name "PollyRylander" party "Rep" race "CommissionerofAgriculture")
(RobertoAron ISA Candidate name "RobertoAron" party "Dem" race "CommissionerofAgriculture")

;;
(JillianBalas ISA Candidate name "JillianBalas" party "Rep" race "RailroadCommissioner")
(ZacharyMinick ISA Candidate name "ZacharyMinick" party "Dem" race "RailroadCommissioner")

;;
(WesleySteven-Millette ISA Candidate name "WesleyStevenMillette" party "Dem" race "StateSenator")
(RicardoNigro ISA Candidate name "RicardoNigro" party "Rep" race "StateSenator")

;;
(SusanneRael ISA Candidate name "SusanneRael" party "Dem" race "StateRepresentativeDistrict134")
(PetraBencomo ISA Candidate name "PetraBencomo" party "Rep" race "StateRepresentativeDistrict134")

;;
(PeterVarga ISA Candidate name "PeterVarga" party "Rep" race "MemberStateBoardofEducationDistrict2")
(MarkBaber ISA Candidate name "MarkBaber" party "Dem" race "MemberStateBoardofEducationDistrict2")

;;
(TimGrasty ISA Candidate name "TimGrasty" party "Dem" race "PresidingJudgeTexasSupremeCourtPlace2")

;;
(Party ISA VoteParty default "Dem")
)


; Chunk base-level activations for intended candidates
(sdp GordonBearce :base-level .3)
(sdp CoreyDery :base-level .3)
(sdp RobertMettler :base-level .3)
;;
(sdp ScottMcCallum :base-level .3)
;;
(sdp RickOrgan :base-level .2)
(sdp ThereseGustin :base-level .2)
(sdp EliseEllzey :base-level .2)
(sdp PollyRylander :base-level .2)
(sdp JillianBalas :base-level .1)
(sdp WesleySteven-Millette :base-level .1)
(sdp SusanneRael :base-level .1)
(sdp PeterVarga :base-level .1)
(sdp TimGrasty :base-level .1)

; Activations for other candidates
(sdp VernonStanleyAlbury :base-level -0.4)
(sdp JanetteFroman :base-level -0.4)
(sdp CecileCadieux :base-level -0.4)
(sdp FernBrzezinski :base-level -0.4)
(sdp PedroBrouse :base-level -0.4)
(sdp TimSpeight :base-level -0.4)
(sdp GregConverse :base-level -0.4)
(sdp SamSaddler :base-level -0.4)
(sdp RobertoAron :base-level -0.4)
(sdp ZacharyMinick :base-level -0.4)
(sdp RicardoNigro :base-level -0.4)
(sdp PetraBencomo :base-level -0.4)
(sdp MarkBaber :base-level -0.4)

;;wisconsin
(sdp JimDoyle :base-level -0.4)
(sdp JimYoung :base-level -0.4)
(sdp EdThompson :base-level -0.4)
(sdp AlanEisenberg :base-level -0.4)
(sdp TyABollerud :base-level -0.4)
(sdp MikeMangan :base-level -0.4)
(sdp AnebJahRasta :base-level -0.4)

; default party chunk
(sdp Vote :base-level .8)