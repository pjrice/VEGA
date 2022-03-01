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
(GlenTravisLozier ISA Candidate name "GlenTravisLozier" party "Rep" race "Governor")
(RickStickles ISA Candidate name "RickStickles" party "Dem" race "Governor")
(MauriceHumble ISA Candidate name "MauriceHumble" party "Ind" race "Governor")
;;
(ShaneTerrio ISA Candidate name "ShaneTerrio" party "Rep" race "LieutenantGovernor")
(CassiePrincipe ISA Candidate name "CassiePrincipe" party "Dem" race "LieutenantGovernor")
;;
(TimSpeight ISA Candidate name "TimSpeight" party "Rep" race "AttorneyGeneral")
(RickOrgan ISA Candidate name "RickOrgan" party "Dem" race "AttorneyGeneral")
;;
(ThereseGustin ISA Candidate name "theresegustin" party "Ind" race "comptrollerofpublicaccounts")
(GregConverse ISA Candidate name "GregConverse" party "Dem" race "comptrollerofpublicaccounts")

;;
(EliseEllzey ISA Candidate name "eliseellzey" party "Dem" race "commissionerofgenerallandoffice")
(SamSaddler ISA Candidate name "SamSaddler" party "Rep" race "commissionerofgenerallandoffice")

;;
(PollyRylander ISA Candidate name "pollyrylander" party "Rep" race "commissionerofagriculture")
(RobertoAron ISA Candidate name "RobertoAron" party "Dem" race "commissionerofagriculture")

;;
(JillianBalas ISA Candidate name "jillianbalas" party "Rep" race "railroadcommissioner")
(ZacharyMinick ISA Candidate name "ZacharyMinick" party "Dem" race "railroadcommissioner")

;;
(WesleySteven-Millette ISA Candidate name "wesleystevenmillette" party "Dem" race "statesenator")
(RicardoNigro ISA Candidate name "RicardoNigro" party "Rep" race "statesenator")

;;
(SusanneRael ISA Candidate name "susannerael" party "Dem" race "stateRepresentativedistrict134")
(PetraBencomo ISA Candidate name "PetraBencomo" party "Rep" race "stateRepresentativedistrict134")

;;
(PeterVarga ISA Candidate name "petervarga" party "Rep" race "memberstateboardofeducationdistrict2")
(MarkBaber ISA Candidate name "MarkBaber" party "Dem" race "memberstateboardofeducationdistrict2")

;;
(TimGrasty ISA Candidate name "timgrasty" party "Dem" race "presidingjudgetexassupremecourtplace2")
;;
(DerrickMelgar ISA Candidate name "derrickmelgar" party "Dem" race "presidingjudgecourtofcriminalappeals")
(DanPlouffe ISA Candidate name "DanPlouffe" party "Rep" race "presidingjudgecourtofcriminalappeals")

;;
(CoreyBehnke ISA Candidate name "coreybehnke" party "Rep" race "districtattorney")
(JenniferALundeed ISA Candidate name "JenniferALundeed" party "Dem" race "districtattorney")

;;
(DeanCaffee ISA Candidate name "deancaffee" party "Rep" race "countytreasurer")
(GordonKallas ISA Candidate name "GordonKallas" party "Dem" race "countytreasurer")

;;
(JasonValle ISA Candidate name "jasonvalle" party "Dem" race "sheriff")
(StanleySaari ISA Candidate name "StanleySaari" party "Rep" race "sheriff")

;;
(HowardGrady ISA Candidate name "howardgrady" party "Rep" race "countytaxassessor")
(RandyHClemons ISA Candidate name "RandyHClemons" party "Dem" race "countytaxassessor")

;;
(ClydeGaytonJr. ISA Candidate name "clydegaytonjr" party "Dem" race "justiceofthepeace")
(DeborahKamps ISA Candidate name "DeborahKamps" party "Rep" race "justiceofthepeace")

;;
(LewisShine ISA Candidate name "lewisshine" party "Dem" race "countyjudge")
(DanAtchley ISA Candidate name "DanAtchley" party "Rep" race "countyjudge")

;;
(Placeholder ISA Candidate name "Candidate2" party "Dem" race "NameofRace")
(Placeholder2 ISA Candidate name "Candidate3" party "Dem" race "LastRace")
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

; Activations for other candidates
(sdp VernonStanleyAlbury :base-level -0.4)
(sdp JanetteFroman :base-level -0.4)
(sdp CecileCadieux :base-level -0.4)
(sdp FernBrzezinski :base-level -0.4)
(sdp PedroBrouse :base-level -0.4)
(sdp GlenTravisLozier :base-level -0.4)
(sdp MauriceHumble :base-level -0.4)
(sdp ShaneTerrio :base-level -0.4)
(sdp TimSpeight :base-level -0.4)
(sdp GregConverse :base-level -0.4)
(sdp SamSaddler :base-level -0.4)
(sdp RobertoAron :base-level -0.4)
(sdp ZacharyMinick :base-level -0.4)
(sdp RicardoNigro :base-level -0.4)
(sdp PetraBencomo :base-level -0.4)
(sdp MarkBaber :base-level -0.4)
(sdp DanPlouffe :base-level -0.4)
(sdp JenniferALundeed :base-level -0.4)
(sdp GordonKallas :base-level -0.4)
(sdp StanleySaari :base-level -0.4)
(sdp RandyHClemons :base-level -0.4)
(sdp DeborahKamps :base-level -0.4)
(sdp DanAtchley :base-level -0.4)

; default party chunk
(sdp Vote :base-level .8)