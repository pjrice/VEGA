;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Joshua Engels
;;; Copyright   : (c) 2019 Joshua Engels
;;; Address     : Lovett College
;;;             : Rice University
;;;             : Houston, TX 77005
;;;             : jae4@rice.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : NLC-Offset-Random.lisp
;;; Version     : 1
;;; 
;;; Description : A ballot
;;;				: * This file establishes an act-r window that represents a complete ballot with no lines seperating the races.
;;;
;;; Bugs        : * None known
;;;
;;; To do       : * The logging does not work if a button is unclicked (because there is no such method yet in logging) 
;;;				: * See logging.lisp todo for more info
;;; 
;;; ----- History -----
;;; 2019.9.27   Joshua Engels
;;;				: * Created the file
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;;
;;; Each race defined in the cntst-lst construction is sequentially put on the ballot from top to bottom and left to right. 
;;; The races are offset from each other with basically arbitrary x and y values (just what made it work), so care should be taken
;;; when writing new ballot functions using this as a model or when changing the races (i.e. adding more candidates would screw things up)
;;; Pressing the buttons causes a log candidate event to occur with the given candidate and other neccesary information (that's what the maps
;;; are for). The ballot is regularly laid out (with no noise), with candidates and parties and buttons sharing the same y value.
;;; Plus colors!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; This first section of code defines the list of races: their names, candidates, and parties, and are used in the second section to construction
;; the ballot
(defparameter cntst-lst nil)

; A single race
(defclass contest ()
  ((office-name :accessor office-name :initarg :office-name :initform nil)

   (cand-lst :accessor cand-lst :initarg :cand-lst :initform nil)
   (selection :accessor selection :initarg :selection :initform nil)
   (office-field :accessor office-field :initarg :office-field)
	)
  )
  

; A single candidate  
(defclass cand-choice ()
  ((cand-name :accessor cand-name :initarg :cand-name :initform nil)
   (party-name :accessor party-name :initarg :party-name :initform "")
   (selected-p :accessor selected-p :initarg :selected-p :initform nil)
   (parent :accessor parent :initarg :parent :initform nil)
   (my-idx :accessor my-idx :initarg :my-idx :initform nil)
   ))
   
 ; From rosetta code
(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)
  
 

; inclusive
(defun randrange (low high)
(+ low (random (- (+ high 1) low)))
)
    
   
(setf all-candidates 
'(
	"DarronGokey"
	"NilsaRusso"
	"ErinFite"
	"VondaFill"
	"ThaddeusShriver"
	"MorrisLoadholt"
	"EmeraldDonadio"
	"CelestaSuzuki"
	"IsabelBrumit"
	"EmeryKahler"
	"JaimePolich"
	"AudreaSizelove"
	"CheyenneScoggin"
	"WilliamWohlwend"
	"ShalonTittle"
	"CaryRosati"
	"JoetteMusselman"
	"AmberWiese"
	"QuentinErb"
	"AndriaHammes"
	"JewelWillams"
	"ClotildeHung"
	"MarqueriteKupfer"
	"KristinaSakamoto"
	"KristleIrby"
	"MargertFavors"
	"ChiMcnerney"
	"IsidroHervey"
	"BritanySkipworth"
	"BilliGastelum"
	"RoslynTassone"
	"LaraBradberry"
	"AmmieRathman"
	"NewtonRickles"
	"ArronDevault"
	"JayneCatania"
	"GerriSwanigan"
	"SuziMcgeehan"
	"ThomasenaAvera"
	"MalkaRogowski"
	"BlossomMoone"
	"FelicitaFesler"
	"WendolynSchermerhorn"
	"ShaquitaBilbrey"
	"PhillisMeyers"
	"DodieDefazio"
	"LavernaCloud"
	"MelvinaPewitt"
	"ElzaKurek"
	"MelanyMillikan"
	"NamBainter"
	"LaraineKnapik"
	"RosalineMckitrick"
	"FrancescoManahan"
	"RoseannSchilke"
	"DarrickMolloy"
	"MorganWelton"
	"NataliaWasmund"
	"GailKunst"
	"EleneHamburger"
	"NatalyaWidner"
	"LymanCarranza"
	"MeiPlatero"
	"OliverHenne"
	"CarminaWebre"
	"HarryWisecup"
	"KaiDelapp"
	"HisakoMerlos"
	"DoriBeauchemin"
	"DonFluellen"
	"MadlynMcaninch"
	"AlmetaShepler"
	"KipXie"
	"TrishFava"
	"LaviniaHupp"
	"OsvaldoOrtez"
	"NichelleLeming"
	"MichalPannone"
	"SharylWomac"
	"YvoneMoench"
	"DelfinaNapoli"
	"OzellaLadue"
	"GillianDufault"
	"JillianGarnes"
	"DeonnaMestas"
	"JoleneZiglar"
	"EdnaMcinnis"
	"KimberelyYbanez"
	"DonteCockett"
	"DenyseHeine"
	"CharlynSchrack"
	"OllieLuck"
	"MarilouTepper"
	"SerinaKendall"
	"StacyScarberry"
	"GertieCoulston"
	"NickoleMckane"
	"ElenorSaeger"
	"FondaJester"
	"SamathaCostas"
	"AlbertaBoles"
	"CedrickZerby"
	"KellyeActon"
	"RolfForan"
	"GlenPippins"
	"MollieTussey"
	"HarryKohn"
	"AlphaGillespie"
	"LesliePhares"
	"SylviaHooper"
	"CarolyneWeich"
	"TawnaBearse"
	"LynnClayborne"
	"HarrietteDifilippo"
	"VerniaHornstein"
	"SheridanDominy"
	"GoldaBattaglia"
	"MarylinSchuetz"
	"DouglassWansley"
	"LakeishaDrake"
	"CharlieMoreles"
	"AnnelleGiusti"
	"LemuelBrantner"
	"DominicaCaggiano"
	"LessieOleary"
	"CorrinPeralez"
	"FatimaDelp"
	"NoreenHenson"
	"JulianaTilman"
	"EmogeneHemsley"
	"JeaniceAlber"
	"MikiVassel"
	"MarivelTullius"
	"LakeshaEves"
	"AlleneBrumbaugh"
	"MarianYarnell"
	"RheaShows"
	"MyrnaAlvarez"
	"CorinnaLehto"
	"AronStephen"
	"LilaBoothby"
	"KaronDouthitt"
	"NewtonDubuc"
	"MarcelineEdmundson"
	"VickiNugent"
	"AleshaKennedy"
	"LoretteFeaster"
	"OpalHector"
	"FernandaBrannum"
	"RaulAndrepont"
	"DaneHigginson"
	"TashaEarheart"
	"ClaudiaMcclintock"
	"MelaniKincaid"
	"GlynisMccabe"
	"LaureneShute"
	"ClaytonEscoto"
	"HannahOrtner"
	"RolandOsman"
	"EleonorTiller"
	"MeghanMater"
	"JanieceNeel"
	"GilLuckie"
	"MorganPulice"
	"MurraySturm"
	"AltonGaffney"
	"GeorgiannBuitron"
	"WilliamsSaini"
	"ShanellDepalma"
	"LigiaKaram"
	"RevaDidier"
	"JackquelineAlmaguer"
	"CindieSalo"
	"LadyAtwood"
	"DebrahFlournoy"
	"WhitleyRowden"
	"RandiMork"
	"MaricelaBreault"
	"CurtisHenrichs"
	"DustinLuis"
	"BarberaNickels"
	"WillyCallihan"
	"RosinaKirtley"
	"EliaPusey"
	"BernardoRailsback"
	"YahairaYaeger"
	"DelmaRing"
	"SongMullenax"
	"HaileyWass"
	"LahomaTabron"
	"VivianaFernando"
	"EddyDetamore"
	"GhislaineQuintanar"
	"TessaHans"
	"MyrtieParkison"
	"WardToki"
	"SharondaAllie"
	"AlexWohlwend"
	"SimonCreighton"
	"LatoyaLewallen"
))


(setf all-parties '("DEM" "REP" "LIB" "IND" "GRE"))
(setf all-races 
'(
	"PresidentoftheUnitedStates"
	"UnitedStatesSenator"
	"UnitedStatesRepresentativeDistrict7"
	"Governor"
	"LieutenantGovernor"
	"AttorneyGeneral"
	"ComptrollerofPublicAccounts"
	"CommissionerofGeneralLandOffice"
	"CommissionerofAgriculture"
	"RailroadCommissioner"
	"StateSenator"
	"StateRepresentativeDistrict134"
	"MemberStateBoardofEducationDistrict2"
	"PresidingJudgeTexasSupremeCourtPlace2"
	"PresidingJudgeCourtofCriminalAppeals"
	"DistrictAttorney"
	"CountyTreasurer"
	"Sheriff"
	"CountyTaxAssessor"
	"JusticeofthePeace"
	"CountyJudge"
	"CityController"
	"BoardOfTrustees"
	"CollegeBoardOfTrustees"
	"CityCouncilAtLargePostion1"
	"CityCouncilAtLargePostion2"
	"CityCouncilAtLargePostion3"
	"CityCouncilDistrictA"
	"CityCouncilDistrictB"
	"CityCouncilDistrictC"
	"CityCouncilDistrictD"
	"CityCouncilDistrictE"
	"CityCouncilDistrictF"
	"CityCouncilDistrictG"
	"CityCouncilDistrictH"
	"CityCouncilDistrictI"
	"CityCouncilDistrictJ"
	"CityCouncilDistrictK"
	"CityCouncilDistrictL"
	"CityCouncilDistrictM"
	"CityCouncilDistrictN"
	"CityCouncilDistrictO"
	"CityCouncilDistrictP"
	"CityCouncilDistrictQ"
	"CityCouncilDistrictR"
	"CityCouncilDistrictS"
	"CityCouncilDistrictT"
))
 
 




(setf all-candidates (nshuffle all-candidates))

(setf min-race-size 1)

(setf max-race-size 7)

(setf race-lengths (loop for x from 1 to (length all-races) collect (randrange min-race-size max-race-size)))

(setf cntst-lst (maplist 
	(lambda (race-length) 
		(make-instance 'contest 
			:office-name (pop all-races)
			:cand-lst 
				(loop for i from 1 to (car race-length) collect 
					(make-instance 'cand-choice
						:cand-name (pop all-candidates) 
						:party-name (nth (- i 1) all-parties)))))
	race-lengths))
	

; Generate all-perfect dm and corresponding sdps
(defparameter all-perfect-dm '(add-dm))
(defparameter all-perfect-sdp '())
(dolist (race cntst-lst) 

	(let* (
		(index (randrange 0 (- (length (cand-lst race)) 1)))
		(candidate (nth index (cand-lst race)))
		)
	
		(setf all-perfect-dm (append 
			all-perfect-dm 
			(list (list (read-from-string (cand-name candidate)) 'ISA 'Candidate 'name (cand-name candidate) 'party (party-name candidate) 'race (office-name race)))
		))
		(setf all-perfect-sdp (append 
			all-perfect-sdp 
			(list (list 'sdp (read-from-string (cand-name candidate)) ':base-level .8))
		))
	)	
)
(setf all-perfect (append '() (list all-perfect-dm) all-perfect-sdp))






;; A ballot function that gets called by combine.lisp and that runs the model (or a human) on the constructed ballot
;; realtime is a boolean that specifies whether to run thge model in real time or not (only pertanent if use-model is true)
;; use-model is a boolean that specifies whether to use the model or allow just human interaction with the ballot
;; visible is a boolean that specifies whether the ballot window should be made visible or not
(defun vote (realtime use-model visible dolog &optional (contest-parameters '(10 20 16)))

	;; Logging
	(defparameter race-sizes '())
	(defparameter race-sizes-column '())
	(defparameter race-indexes-voted-on '())
	(defparameter race-parameters contest-parameters)

  
	; Resets the act r enviroment
	(reset)
  
  
	; Constructs the window and populates it with race-titles, candidates, parties, and buttons
	(let* (
	
		(y-spacing-between-races (nth 0 contest-parameters))
		(y-spacing-after-title (nth 1 contest-parameters))
		(y-spacing-between-candidates (nth 2 contest-parameters))
		
		(wheight 600)
		(wwidth 800)
		(window (open-exp-window "Ballet" :width wwidth :height wheight :visible visible)) 
		(starting-x 10)
		(starting-y 10)
		(current-x starting-x) 
		(current-y starting-y)
		(x-offset-button 0)
		(x-offset-candidate 30)
		(x-offset-party 200)
		(column-width 300)
		
		; Maps the buttons to various objects so that these values can be accessed in the click response method, where we only have accessed
		; to the object itself
		(button-map (make-hash-table)) ; Maps the buttons to the array of candidate and party objects on screen (to use to change their color to blue)
		(button-state (make-hash-table)) ; Maps the buttons to their state (to know whether to set to blue or black)
		(button-index (make-hash-table)) ; Maps the button to their race's index in cntst-lst 
		(button-candidate (make-hash-table)) ; Maps the button to their associated candidate object from cntst-lst
		(button-race (make-hash-table))) ; Maps the button to their race index

	
		; Places all of the races on the screen
		; Column loop
		(loop		
		
			(setf current-column-sizes '())
		
			; Item low
			(loop 	
			
								
				; Constructs the ballot
				(let* (
				
					; initialization to build the race
					(candidate-party-object-array (make-array (* max-race-size 2)))
					(contest (pop cntst-lst))
					(candidates (cand-lst contest))
					(candidate (pop candidates))
					(index 0)
					(button_temp nil))					

					; Log creation of race
					(setf race-sizes (append race-sizes (list (+ (list-length candidates) 1))))
					(setf current-column-sizes (append current-column-sizes (list (+ (list-length candidates) 1))))
					
					
					; Adds the race name, increment current y
					(add-text-to-exp-window window (office-name contest) :color 'red :x current-x :y current-y)
					(setf current-y (+ current-y y-spacing-after-title))

				
					; Constructs the rest of the ballot
					(loop while candidate 
			
					do 	(progn 
						
						; Displays and stores candidates
						(setf (aref candidate-party-object-array index) (add-text-to-exp-window window (cand-name candidate) :color 'purple :x (+ x-offset-candidate current-x) :y current-y))
						
						; Displays and stores parties
						(setf (aref candidate-party-object-array (+ index 3)) (add-text-to-exp-window window (party-name candidate) :color 'blue :x (+ x-offset-party current-x) :y current-y))

						; Displays and stores buttons
						(setf button_temp (add-button-to-exp-window window :color 'white :text "" :x current-x :y (+ current-y 1) :width 20 :height 10 :action 
						(lambda (button)
						(if (= (gethash button button-state) 0) 
							(progn
								(modify-button-for-exp-window button :color 'black)
								(proc-display)
								(setf (gethash button button-state) 1)
								(setf race-indexes-voted-on (append race-indexes-voted-on (list (gethash button button-race)))))
							(progn
								(modify-button-for-exp-window button :color 'white)
								(proc-display)
								(setf (gethash button button-state) 0))))))	

						; Button information, stored in hashmaps
						(setf (gethash button_temp button-map) candidate-party-object-array)
						(setf (gethash button_temp button-state) 0)
						(setf (gethash button_temp button-index) index)
						(setf (gethash button_temp button-candidate) candidate)
						(setf (gethash button_temp button-race) (- (length race-sizes) 1))


						
						; Loop increment operations
						(setf current-y (+ current-y y-spacing-between-candidates))
						(setf candidate (pop candidates))
						(setf index (+ index 1)))
						))
			
				(when (not cntst-lst) (return)) ;checks if we've run out of races

				(setf current-y (+ current-y y-spacing-between-races))
				(setf next-cntst (car cntst-lst))
				(when (> 
					(+ current-y y-spacing-after-title  (* (length (cand-lst next-cntst)) y-spacing-between-candidates)) 
					wheight)
					(return)) ; y too big

		
			)
			
				
		; More logging
		(setf race-sizes-column (append race-sizes-column (list current-column-sizes)))
		
		(setf current-x (+ current-x column-width))
		(setf current-y	starting-y)
		
		(when (not cntst-lst) (return)) ;checks if we've run out of races
		(when (> current-x wwidth) (return)) ;checks if we've run out of left right room

	)
	
	;this should be removed in the future; for testing only (it is called if the if statement below)
	(install-device window)
	
	; Runs the model for 300 seconds on the ballot if use-model is true
	(if use-model
		(progn
		(install-device window)
		(proc-display)
		(start-hand-at-mouse)
		(if realtime (run 300 t) (run 300))
		;(if dolog (log-ballot))
		))
))
