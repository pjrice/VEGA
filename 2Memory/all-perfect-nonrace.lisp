

(add-dm

;; knowledge of instructions
(Ballot ISA Instruction keyword "ballot" area "Header")
(Style ISA Instruction keyword "style" area "Header")
(County ISA Instruction keyword "county" area "Header")
(November ISA Instruction keyword "november" area "Header")
(Day ISA Instruction keyword "5" area "Header")
(Year ISA Instruction keyword "2002" area "Header")
(Kewaunee ISA Instruction keyword "kewaunee" area "Header")
(State ISA Instruction keyword "state" area "Header")
(Wisconsin ISA Instruction keyword "wisconsin" area "Header")

(Instrs ISA Instruction keyword "instructions" area "Instructions")
(Initialed ISA Instruction keyword "initialed" area "Instructions")
(Instr ISA Instruction keyword "instruction" area "Instructions")
(Official ISA Instruction keyword "official" area "Instructions")
(General ISA Instruction keyword "general" area "Instructions")
(Election ISA Instruction keyword "election" area "Instructions")
(Referendum ISA Instruction keyword "Referendum" area "Instructions")
(Notice ISA Instruction keyword "notice" area "Instructions")
(Electors ISA Instruction keyword "electors" area "Instructions")
(Vote ISA Instruction keyword "vote" area "Instructions")
(Blacken ISA Instruction keyword "blacken" area "Instructions")
(Oval ISA Instruction keyword "oval" area "Instructions")
(Candidates ISA Instruction keyword "candidates" area "Instructions")
(Independent ISA Instruction keyword "independent" area "Instructions")
(Important ISA Instruction keyword "important" area "Instructions")
(Left ISA Instruction keyword "left" area "Instructions")
(Line ISA Instruction keyword "line" area "Instructions")
(Manucipal ISA Instruction keyword "manucipal" area "Instructions")
(Deputy ISA Instruction keyword "deputy" area "Instructions")
(Clerk ISA Instruction keyword "clerk" area "Instructions")
(Congressional ISA Instruction keyword "congressional" area "Instructions")
(Legistiative ISA Instruction keyword "legistiative" area "Instructions")
(Ticket ISA Instruction keyword "ticket" area "Instructions")
(Absentee ISA Instruction keyword "absentee" area "Instructions")
(Party ISA Instruction keyword "party" area "Instructions")
;(Of ISA Instruction keyword "of" area "Instructions")
;(Th ISA Instruction keyword "the" area "Instructions")
(Jointly ISA Instruction keyword "jointly" area "Instructions")
(Only ISA Instruction keyword "only" area "Instructions")
(Name ISA Instruction keyword "name" area "Instructions")
;(For ISA Instruction keyword "for" area "Instructions")
(Write ISA Instruction keyword "write" area "Instructions")
(Person ISA Instruction keyword "person" area "Instructions")

;(To ISA Instruction keyword "to" area "Footer")
(Carlton ISA Instruction keyword "carlton" area "Footer")
(Town ISA Instruction keyword "town" area "Footer")
(Continue ISA Instruction keyword "continue" area "Footer")
(Voting ISA Instruction keyword "voting" area "Footer")
(Please ISA Instruction keyword "please" area "Footer")
(Turn ISA Instruction keyword "turn" area "Footer")
;(Over ISA Instruction keyword "over" area "Footer")

;; candidates
;;wisconsin error
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
(sdp ScottMcCallum :base-level .8)
;;
(sdp BillNelson :base-level .8)
(sdp AlceeLHastings :base-level .8)
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


; Chunk base-level activations for non-race content
(sdp Ballot :base-level 10)
(sdp Style :base-level 10)
(sdp County :base-level 10)
(sdp November :base-level 10)
(sdp Day :base-level 10)
(sdp Year :base-level 10)
(sdp Kewaunee :base-level 10)
(sdp State :base-level 10)
(sdp Wisconsin :base-level 10)

(sdp Instr :base-level 10)
(sdp Instrs :base-level 10)
(sdp Initialed :base-level 10)
(sdp Official :base-level 10)
(sdp General :base-level 10)
(sdp Election :base-level 10)
(sdp Referendum :base-level 10)
(sdp Notice :base-level 10)
(sdp Electors :base-level 10)
(sdp Vote :base-level 10)
(sdp Blacken :base-level 10)
(sdp Oval :base-level 10)
(sdp Write :base-level 10)

(sdp Party :base-level 10)
(sdp Absentee :base-level 10)
(sdp Ticket :base-level 10)
(sdp Congressional :base-level 10)
(sdp Legistiative :base-level 10)
(sdp Clerk :base-level 10)
(sdp Deputy :base-level 10)
(sdp Manucipal :base-level 10)
(sdp Line :base-level 10)
(sdp Important :base-level 10)
(sdp Left :base-level 10)
(sdp Legistiative :base-level 10)
(sdp Independent :base-level 10)
(sdp Candidates :base-level 10)
(sdp Person :base-level 10)

;(sdp Of :base-level 20)
;(sdp Th :base-level 20)
(sdp Jointly :base-level 10)
(sdp Only :base-level 10)
(sdp Name :base-level 10)
;(sdp For :base-level 20)

;(sdp To :base-level 20)
(sdp Carlton :base-level 10)
(sdp Town :base-level 10)
(sdp Continue :base-level 10)
(sdp Voting :base-level 10)
(sdp Please :base-level 10)
(sdp Turn :base-level 10)
(sdp Over :base-level 10)




