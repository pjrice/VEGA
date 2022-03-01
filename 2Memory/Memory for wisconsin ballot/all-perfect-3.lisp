
(add-dm

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
(WesleySteven-Millette ISA Candidate name "WesleyStevenMillette" party "Dem" race "StateSenator")
;;
(SusanneRael ISA Candidate name "SusanneRael" party "Dem" race "StateRepresentativeDistrict134")
;;
(PeterVarga ISA Candidate name "PeterVarga" party "Rep" race "MemberStateBoardofEducationDistrict2")
;;
(TimGrasty ISA Candidate name "TimGrasty" party "Dem" race "PresidingJudgeTexasSupremeCourtPlace2")
;;
(Party ISA VoteParty default "Dem")
)

; Chunk base-level activations for intended candidates
(sdp GordonBearce :base-level .2)
(sdp CoreyDery :base-level .2)
(sdp RobertMettler :base-level .2)
;;
(sdp ScottMcCallum :base-level .2)
;;
(sdp RickOrgan :base-level .2)
(sdp ThereseGustin :base-level .2)
(sdp EliseEllzey :base-level .2)
(sdp PollyRylander :base-level .2)
(sdp JillianBalas :base-level .2)
(sdp WesleyStevenMillette :base-level .2)
(sdp SusanneRael :base-level .2)
(sdp PeterVarga :base-level .2)
(sdp TimGrasty :base-level .2)

; default party chunk
(sdp Vote :base-level .8)


; no Activations for other candidates



