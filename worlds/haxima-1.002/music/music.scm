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
   "music/audionetwork/ANW1857_28_The-Flag-13-(Sting).wav"
   "music/audionetwork/ANW1520_66_This-Glorious-Land-9-(Sting).wav"
   "music/audionetwork/ANW2105_77_To-The-Rescue-6-(Sting).wav"
   ))

;; These should be a short fanfare
(define ml-battle-over
  (music-list
   "music/audionetwork/ANW1520_36_For-King-And-Country-7-(Sting).wav"
   "music/audionetwork/ANW2105_78_To-The-Rescue-7-(Sting).wav"
   "music/audionetwork/ANW1857_24_The-Flag-9-(Sting).wav"
   ))

(define ml-battle-music
	(music-list
	 "music/audionetwork/ANW2169_05_Tense-Realisation.mp3"
	 "music/audionetwork/ANW2087_05_Brooding-Thoughts.mp3"
	 "music/audionetwork/ANW2068_22_Lethal-Weapon-3.mp3"
	 "music/audionetwork/ANW2196_132_Make-Or-Break-8-(60).mp3"
	 "music/audionetwork/ANW2110_01_Determined.mp3"
))

(define ml-outdoor-adventure
	(music-list
"music/double-trios.ogg"
;;"music/ballad.ogg"
))

(define ml-dungeon-adventure
	(music-list
"music/double-trios.ogg"
;;"music/ballad.ogg"	
))

(define ml-creepy-area
	(music-list
"music/double-trios.ogg"
;;"music/ballad.ogg"	
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
;;"music/plainchant-recorder-trio.ogg"
;;"music/minstrel-dance.ogg"
;;"music/Minuet-like-Mozart.ogg"
))

(define ml-large-town
	(music-list
;;"music/bassoons-and-harpsichord.ogg"
;;"music/Minuet-like-Mozart.ogg"
))

(define ml-castle
	(music-list
"music/audionetwork/ANW2166_16_Quiet-Altitude-2-(60).mp3"
"music/audionetwork/ANW2166_21_Acoustic-Hymn-4-(60).mp3"
))

(define ml-dungeon-town
	(music-list
"music/double-trios.ogg"
;;"music/ballad.ogg"	
))

(define ml-sailing
	(music-list
"music/wind-trio.ogg"
))

(define ml-moongate-clearing
  (music-list
   "music/audionetwork/ANW2038_14_Serene-Scene.mp3"
   ))

(define ml-char-setup
  (music-list
   "music/audionetwork/ANW1082_10_Princes-In-The-Tower.mp3"
   ))

(define ml-dark-suspense
  (music-list
   "/home/gmcnutt/git/nazghul/worlds/haxima-1.002/music/audionetwork/ANW2175_79_Suspense-5.mp3"
   "/home/gmcnutt/git/nazghul/worlds/haxima-1.002/music/audionetwork/ANW2175_80_Suspense-6.mp3"
   ))