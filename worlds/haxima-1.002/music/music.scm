;; You can replace these with your own music files if you like.
;; It should be reasonably easy to keep thematically correct.
;; You can use full paths for stuff outside the haxima directory
;; Use '/' instead of '\' unless you know better

;; SDL doesnt seem to like all mp3s- may be some encoding issues or something
;; I get some playing half speed, turning a fast paced combat song into something
;; more suitable for 'the Morgue of Satan'

;; These should be a short fanfare
(define ml-battle-intro
  (music-list
   "music/audionetwork/ANW1857_28_The-Flag-13-(Sting).mp3"
   "music/audionetwork/ANW1520_66_This-Glorious-Land-9-(Sting).mp3"
   "music/audionetwork/ANW2105_77_To-The-Rescue-6-(Sting).mp3"
   ))

;; These should be a short fanfare
(define ml-battle-over
  (music-list
   "music/audionetwork/ANW1520_36_For-King-And-Country-7-(Sting).mp3"
   "music/audionetwork/ANW2105_78_To-The-Rescue-7-(Sting).mp3"
   "music/audionetwork/ANW1857_24_The-Flag-9-(Sting).mp3"
   ))

(define ml-battle-music
  (music-list
   "music/audionetwork/ANW2169_05_Tense-Realisation.mp3"
   "music/audionetwork/ANW2087_05_Brooding-Thoughts.mp3"
   "music/audionetwork/ANW2068_22_Lethal-Weapon-3.mp3"
   ))

(define ml-outdoor-adventure
  ;; Used for dangerous, somewhat creepy or ominous locations
  (music-list
   "music/audionetwork/ANW2007_01_Campaign.mp3"

   "music/audionetwork/ANW2175_81_Suspense-7.mp3"
))

(define ml-dungeon-adventure
  (music-list
   "music/audionetwork/ANW1082_10_Princes-In-The-Tower.mp3"
   "music/audionetwork/ANW1254_05_Burned-Earth.mp3"
   "music/audionetwork/ANW2169_18_Unfinished-Business-3.mp3"
   "music/audionetwork/ANW2171_03_Betrayed.mp3"
   "music/audionetwork/ANW1500_06_Nevada-Snakebite.mp3"
))

(define ml-creepy-area
  (music-list
   "music/audionetwork/ANW2175_79_Suspense-5.mp3"
   "music/audionetwork/ANW2175_80_Suspense-6.mp3"
))

(define ml-travelling
  (music-list
   "music/audionetwork/ANW2152_05_Final-Road.mp3"
   "music/audionetwork/ANW1086_01_Back-Canyon.mp3"
   "music/audionetwork/ANW2211_01_Journey-Begins.mp3"
   "music/audionetwork/ANW1877_03_Signals.mp3"
))

(define ml-small-town
  (music-list
   "music/audionetwork/ANW1985_37_Glue-4-(60).mp3"
   "music/audionetwork/ANW2209_13_Four-Leaf-Clover-8-(30).mp3"
))

(define ml-large-town
  (music-list
   "music/audionetwork/ANW1097_06_Autumn.mp3"
   ;;"music/audionetwork/ANW1520_47_Market-Town-5-(30).mp3"
   ))

(define ml-castle
  (music-list
   "music/audionetwork/ANW2166_16_Quiet-Altitude-2-(60).mp3"
   "music/audionetwork/ANW2166_21_Acoustic-Hymn-4-(60).mp3"
))

(define ml-dungeon-town
  (music-list
   "music/audionetwork/ANW2110_01_Determined.mp3"
))

(define ml-sailing
  (music-list
   "music/audionetwork/ANW1120_08_Hornpipe.mp3"
   "music/audionetwork/ANW2202_37_Silver's-Set-4-(60).mp3"
   "music/audionetwork/ANW2202_23_All-At-Sea-3.mp3"
   "music/audionetwork/ANW2202_97_John-Silver\'s-Dance-3.mp3"
))

(define ml-peaceful-area
  (music-list
   "music/audionetwork/ANW2038_14_Serene-Scene.mp3"
   "music/audionetwork/ANW1132_06_New-Age.mp3"
   "music/audionetwork/ANW1097_06_Autumn.mp3"
   "music/audionetwork/ANW2214_29_Angels-In-The-Breeze-7-\(60\).mp3"
   ))

(define ml-char-setup
  (music-list
   "music/audionetwork/ANW1082_10_Princes-In-The-Tower.mp3"
   ))

(define ml-void
  (music-list
   "music/audionetwork/ANW2169_27_Space-Infinity-3.mp3"
   "music/audionetwork/ANW2213_11_Like-A-Ghost-2.mp3"
   ))

(define ml-sea-shanties
  (music-list
   "music/audionetwork/ANW1855_08_Jolly-Sailor.mp3"
   "music/audionetwork/ANW2202_16_Shore-Leave-7-(30).mp3"
   "music/audionetwork/ANW2202_69_Rock-The-Boat-9-\(30\).mp3"
  ))

(define ml-final-battle
  (music-list
   "music/audionetwork/ANW2052_01_Crash.mp3"
   ))