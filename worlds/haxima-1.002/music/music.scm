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
   "music/audionetwork/ANW1857_28_The-Flag-13-Sting.ogg"
   "music/audionetwork/ANW1520_66_This-Glorious-Land-9-Sting.ogg"
   "music/audionetwork/ANW2105_77_To-The-Rescue-6-Sting.ogg"
   ))

;; These should be a short fanfare
(define ml-battle-over
  (music-list
   "music/audionetwork/ANW1520_36_For-King-And-Country-7-Sting.ogg"
   "music/audionetwork/ANW2105_78_To-The-Rescue-7-Sting.ogg"
   "music/audionetwork/ANW1857_24_The-Flag-9-Sting.ogg"
   ))

(define ml-battle-music
  (music-list
   "music/audionetwork/ANW2169_05_Tense-Realisation.ogg"
   "music/audionetwork/ANW2087_05_Brooding-Thoughts.ogg"
   "music/audionetwork/ANW2068_22_Lethal-Weapon-3.ogg"
   ))

(define ml-outdoor-adventure
  ;; Used for dangerous, somewhat creepy or ominous locations
  (music-list
   "music/audionetwork/ANW2007_01_Campaign.ogg"

   "music/audionetwork/ANW2175_81_Suspense-7.ogg"
))

(define ml-dungeon-adventure
  (music-list
   "music/audionetwork/ANW1082_10_Princes-In-The-Tower.ogg"
   "music/audionetwork/ANW1254_05_Burned-Earth.ogg"
   "music/audionetwork/ANW2169_18_Unfinished-Business-3.ogg"
   "music/audionetwork/ANW2171_03_Betrayed.ogg"
   "music/audionetwork/ANW1500_06_Nevada-Snakebite.ogg"
))

(define ml-creepy-area
  (music-list
   "music/audionetwork/ANW2175_79_Suspense-5.ogg"
   "music/audionetwork/ANW2175_80_Suspense-6.ogg"
))

(define ml-travelling
  (music-list
   "music/audionetwork/ANW2152_05_Final-Road.ogg"
   "music/audionetwork/ANW1086_01_Back-Canyon.ogg"
   "music/audionetwork/ANW2211_01_Journey-Begins.ogg"
   "music/audionetwork/ANW1877_03_Signals.ogg"
))

(define ml-small-town
  (music-list
   "music/audionetwork/ANW1985_37_Glue-4-60.ogg"
   "music/audionetwork/ANW2209_13_Four-Leaf-Clover-8-30.ogg"
))

(define ml-large-town
  (music-list
   "music/audionetwork/ANW1097_06_Autumn.ogg"
   ;;"music/audionetwork/ANW1520_47_Market-Town-5-30.ogg"
   ))

(define ml-castle
  (music-list
   "music/audionetwork/ANW2166_16_Quiet-Altitude-2-60.ogg"
   "music/audionetwork/ANW2166_21_Acoustic-Hymn-4-60.ogg"
))

(define ml-dungeon-town
  (music-list
   "music/audionetwork/ANW2110_01_Determined.ogg"
))

(define ml-sailing
  (music-list
   "music/audionetwork/ANW1120_08_Hornpipe.ogg"
   "music/audionetwork/ANW2202_37_Silvers-Set-4-60.ogg"
   "music/audionetwork/ANW2202_23_All-At-Sea-3.ogg"
   "music/audionetwork/ANW2202_97_John-Silvers-Dance-3.ogg"
))

(define ml-peaceful-area
  (music-list
   "music/audionetwork/ANW2038_14_Serene-Scene.ogg"
   "music/audionetwork/ANW1132_06_New-Age.ogg"
   "music/audionetwork/ANW1097_06_Autumn.ogg"
   "music/audionetwork/ANW2214_29_Angels-In-The-Breeze-7-60.ogg"
   ))

(define ml-char-setup
  (music-list
   "music/audionetwork/ANW1082_10_Princes-In-The-Tower.ogg"
   ))

(define ml-void
  (music-list
   "music/audionetwork/ANW2169_27_Space-Infinity-3.ogg"
   "music/audionetwork/ANW2213_11_Like-A-Ghost-2.ogg"
   ))

(define ml-sea-shanties
  (music-list
   "music/audionetwork/ANW1855_08_Jolly-Sailor.ogg"
   "music/audionetwork/ANW2202_16_Shore-Leave-7-30.ogg"
   "music/audionetwork/ANW2202_69_Rock-The-Boat-9-30.ogg"
  ))

(define ml-final-battle
  (music-list
   "music/audionetwork/ANW2052_01_Crash.ogg"
   ))