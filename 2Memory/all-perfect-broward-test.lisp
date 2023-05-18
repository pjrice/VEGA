
(add-dm

;; candidates
(ScottMcCallum ISA Candidate name "ScottMcCallum" party "REP" race "GovernorLieutenantGovernor")
;;
(BillNelson ISA Candidate name "BillNelson" party "DEM" race "UnitedStatesSenator")

(AlceeLHastings ISA Candidate name "AlceeLHastings" party "DEM" race "RepresentativeInCongress")
;;
(RobertMettler ISA Candidate name "RobertMettler" party "Dem" race "UnitedStatesRepresentativeD7")
;;
(RickOrgan ISA Candidate name "RickOrgan" party "Dem" race "AttorneyGeneral")
;;
(ThereseGustin ISA Candidate name "ThereseGustin" party "Ind" race "ComptrollerofPublicAccounts")
;;
(PollyRylander ISA Candidate name "PollyRylander" party "Rep" race "CommissionerofAgriculture")
;;
(JillianBalas ISA Candidate name "JillianBalas" party "Rep" race "RailroadCommissioner")
;;
(WesleyMillette ISA Candidate name "WesleyMillette" party "Dem" race "StateSenator")
;;
(SusanneRael ISA Candidate name "SusanneRael" party "Dem" race "StateRepresentativeD134")
;;
(PeterVarga ISA Candidate name "PeterVarga" party "Rep" race "JusticeofthePeace")
;;
(MarkBaber ISA Candidate name "MarkBaber" party "Dem" race "MemberStateBoardofEducationD2")
;;
(TimGrasty ISA Candidate name "TimGrasty" party "Dem" race "CityController")
;;
(LuisKim ISA Candidate name "LuisKim" party "Dem" race "CountyJudge")
;;
(TiffanyPerry ISA Candidate name "TiffanyPerry" party "REP" race "CommissionerofGeneralLandOffice")
;;
(GordonKallas ISA Candidate name "GordonKallas" party "Dem" race "CountyTreasurer")
;;
(JasonValle ISA Candidate name "JasonValle" party "Dem" race "Sheriff")
;;
(Party ISA VoteParty default "Dem")
)

; Chunk base-level activations for intended candidates

(sdp BillNelson :base-level .8)
(sdp AlceeLHastings :base-level .01)
(sdp ScottMcCallum :base-level .8)
(sdp RobertMettler :base-level .8)
(sdp RickOrgan :base-level .8)

(sdp ThereseGustin :base-level .8)
(sdp PollyRylander :base-level .8)
(sdp JillianBalas :base-level .8)
(sdp WesleyMillette :base-level .8)

(sdp SusanneRael :base-level .8)
(sdp PeterVarga :base-level .8)
(sdp MarkBaber :base-level .8)
(sdp TimGrasty :base-level .8)

(sdp LuisKim :base-level .8)
(sdp TiffanyPerry :base-level .8)
(sdp GordonKallas :base-level .8)
(sdp JasonValle :base-level .8)

; default party chunk
(sdp Vote :base-level .8)


; no Activations for other candidates



