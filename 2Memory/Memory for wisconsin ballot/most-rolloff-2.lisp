; DM only includes 70% of the ballot with roll-off activations 
; intentionally abstains for last 30% of the ballot

(add-dm

;****************************************
;****************************************

;Abstain Chunks (abstain from these races)
; last 30% of the ballot

;****************************************
;****************************************

(statesenator ISA Abstain contest "StateSenator")
(staterepresentativedistrict134 ISA Abstain contest "StateRepresentativeDistrict134")
(memberstateboardofeducationdistrict2 ISA Abstain contest "MemberStateBoardofEducationDistrict2")
(presidingjudgetexassupremecourtplace2 ISA Abstain contest "PresidingJudgeTexasSupremeCourtPlace2")

;****************************************
;****************************************

;Candidate Chunks

;****************************************
;****************************************

(GordonBearce ISA Candidate name "GordonBearce" party "Rep" race "PresidentoftheUnitedStates")
;;
(CoreyDery ISA Candidate name "CoreyDery" party "Ind" race "UnitedStatesSenator")
;;
(RobertMettler ISA Candidate name "RobertMettler" party "Dem" race "UnitedStatesRepresentativeDistrict7")
;;
;;wisconsin
(ScottMcCallum ISA Candidate name "ScottMcCallum" party "REP" race "GovernorLieutenantGovernor")
;;
;;
(RickOrgan ISA Candidate name "RickOrgan" party "Dem" race "AttorneyGeneral")
;;
(ThereseGustin ISA Candidate name "ThereseGustin" party "Ind" race "ComptrollerofPublicAccounts")
;;
(EliseEllzey ISA Candidate name "EliseEllzey" party "Dem" race "CommissionerofGeneralLandOffice")
;;
(PollyRylander ISA Candidate name "PollyRylander" party "Rep" race "CommissionerofAgriculture")
;;
(JillianBalas ISA Candidate name "JillianBalas" party "Rep" race "RailroadCommissioner")
;;
(Party ISA VoteParty default "Dem")

)

; Chunk base-level activations for Abstain
;; abstain from last 30%

(sdp statesenator :base-level 2.0)
(sdp staterepresentativedistrict134 :base-level 2.0)
(sdp memberstateboardofeducationdistrict2 :base-level 2.0)
(sdp presidingjudgetexassupremecourtplace2 :base-level 2.0)

; Chunk base-level activations for intended candidates
(sdp GordonBearce :base-level .6)
(sdp CoreyDery :base-level .6)
(sdp RobertMettler :base-level .6)
;;
(sdp ScottMcCallum :base-level .5)
;;
(sdp RickOrgan :base-level .5)
(sdp ThereseGustin :base-level .4)
(sdp EliseEllzey :base-level .4)
(sdp PollyRylander :base-level .3)
(sdp JillianBalas :base-level .3)

; default party chunk
(sdp Vote :base-level .8)


