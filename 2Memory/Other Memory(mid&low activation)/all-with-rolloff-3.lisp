
(add-dm

(GordonBearce ISA Candidate name "GordonBearce" party "Rep" race "PresidentoftheUnitedStates")
;;
(CoreyDery ISA Candidate name "CoreyDery" party "Ind" race "UnitedStatesSenator")
;;
(RobertMettler ISA Candidate name "RobertMettler" party "Dem" race "UnitedStatesRepresentativeDistrict7")
;;
(RickStickles ISA Candidate name "RickStickles" party "Dem" race "Governor")
;;
(CassiePrincipe ISA Candidate name "CassiePrincipe" party "Dem" race "LieutenantGovernor")
;;
(RickOrgan ISA Candidate name "RickOrgan" party "Dem" race "AttorneyGeneral")
;;
(ThereseGustin ISA Candidate name "theresegustin" party "Ind" race "comptrollerofpublicaccounts")
;;
(EliseEllzey ISA Candidate name "eliseellzey" party "Dem" race "commissionerofgenerallandoffice")
;;
(PollyRylander ISA Candidate name "pollyrylander" party "Rep" race "commissionerofagriculture")
;;
(JillianBalas ISA Candidate name "jillianbalas" party "Rep" race "railroadcommissioner")
;;
(WesleySteven-Millette ISA Candidate name "wesleystevenmillette" party "Dem" race "statesenator")
;;
(SusanneRael ISA Candidate name "susannerael" party "Dem" race "stateRepresentativedistrict134")
;;
(PeterVarga ISA Candidate name "petervarga" party "Rep" race "memberstateboardofeducationdistrict2")
;;
(TimGrasty ISA Candidate name "timgrasty" party "Dem" race "presidingjudgetexassupremecourtplace2")
;;
(DerrickMelgar ISA Candidate name "derrickmelgar" party "Dem" race "presidingjudgecourtofcriminalappeals")
;;
(CoreyBehnke ISA Candidate name "coreybehnke" party "Rep" race "districtattorney")
;;
(DeanCaffee ISA Candidate name "deancaffee" party "Rep" race "countytreasurer")
;;
(JasonValle ISA Candidate name "jasonvalle" party "Dem" race "sheriff")
;;
(HowardGrady ISA Candidate name "howardgrady" party "Rep" race "countytaxassessor")
;;
(ClydeGaytonJr. ISA Candidate name "clydegaytonjr" party "Dem" race "justiceofthepeace")
;;
(LewisShine ISA Candidate name "lewisshine" party "Dem" race "countyjudge")
;;
(Party ISA VoteParty default "Dem")
)


; Chunk base-level activations for intended candidates
(sdp GordonBearce :base-level .3)
(sdp CoreyDery :base-level .3)
(sdp RobertMettler :base-level .3)
(sdp RickStickles :base-level .3)
(sdp CassiePrincipe :base-level .3)
(sdp RickOrgan :base-level .3)
(sdp ThereseGustin :base-level .3)

(sdp EliseEllzey :base-level .2)
(sdp PollyRylander :base-level .2)
(sdp JillianBalas :base-level .2)
(sdp WesleySteven-Millette :base-level .2)
(sdp SusanneRael :base-level .2)
(sdp PeterVarga :base-level .2)
(sdp TimGrasty :base-level .2)

(sdp DerrickMelgar :base-level 0.1)
(sdp CoreyBehnke :base-level 0.1)
(sdp DeanCaffee :base-level 0.1)
(sdp JasonValle :base-level 0.1)
(sdp HowardGrady :base-level 0.1)
(sdp ClydeGaytonJr. :base-level 0.1)
(sdp LewisShine :base-level 0.1)

; no Activations for other candidates

; default party chunk
(sdp Vote :base-level .8)

