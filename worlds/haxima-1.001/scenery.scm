;; mk-scenery-type is rather like mk-reagent-type, 
;; but on the terrain features layer, and without an interface.
;; 
;; In future, it will be needful to have some kind of interface --
;; it is an explicit goal in Haxima to enable the player to interact with 
;; the world (and its' objects) whenever reasonable.
(define (mk-scenery-type tag name sprite)
  (mk-obj-type tag name sprite layer-tfeat nil))

;; Lightning bolts:
(mk-scenery-type 't_lightning_bolt_red        "lightning bolt" s_lightning_bolt_red)
(mk-scenery-type 't_lightning_bolt_green      "lightning bolt" s_lightning_bolt_green)
(mk-scenery-type 't_lightning_bolt_blue       "lightning bolt" s_lightning_bolt_blue)
(mk-scenery-type 't_lightning_bolt_cyan       "lightning bolt" s_lightning_bolt_cyan)
(mk-scenery-type 't_lightning_bolt_purple     "lightning bolt" s_lightning_bolt_purple)
(mk-scenery-type 't_lightning_bolt_yellow     "lightning bolt" s_lightning_bolt_yellow)
(mk-scenery-type 't_lightning_bolt_brown      "lightning bolt" s_lightning_bolt_brown)
(mk-scenery-type 't_lightning_bolt_orange     "lightning bolt" s_lightning_bolt_orange)
(mk-scenery-type 't_lightning_bolt_white      "lightning bolt" s_lightning_bolt_white)
(mk-scenery-type 't_lightning_bolt_grey       "lightning bolt" s_lightning_bolt_grey)
(mk-scenery-type 't_lightning_bolt_dark_grey  "lightning bolt" s_lightning_bolt_dark_grey)
(mk-scenery-type 't_lightning_bolt_chromatic  "lightning bolt" s_lightning_bolt_chromatic)
;; Magic ball (fireball core or similar):
(mk-scenery-type 't_fireball_red        "fireball" s_magic_ball_core_red)
(mk-scenery-type 't_fireball_green      "fireball" s_magic_ball_core_green)
(mk-scenery-type 't_fireball_blue       "fireball" s_magic_ball_core_blue)
(mk-scenery-type 't_fireball_cyan       "fireball" s_magic_ball_core_cyan)
(mk-scenery-type 't_fireball_purple     "fireball" s_magic_ball_core_purple)
(mk-scenery-type 't_fireball_yellow     "fireball" s_magic_ball_core_yellow)
(mk-scenery-type 't_fireball_brown      "fireball" s_magic_ball_core_brown)
(mk-scenery-type 't_fireball_orange     "fireball" s_magic_ball_core_orange)
(mk-scenery-type 't_fireball_white      "fireball" s_magic_ball_core_white)
(mk-scenery-type 't_fireball_grey       "fireball" s_magic_ball_core_grey)
(mk-scenery-type 't_fireball_dark_grey  "fireball" s_magic_ball_core_dark_grey)
(mk-scenery-type 't_fireball_chromatic  "fireball" s_magic_ball_core_chromatic)
;; Magic burst/bloom/aura/field (5 ball lightnings in one tile):
(mk-scenery-type 't_magic_field_red           "magic field" s_magic_field_red)
(mk-scenery-type 't_magic_field_green         "magic field" s_magic_field_green)
(mk-scenery-type 't_magic_field_blue          "magic field" s_magic_field_blue)
(mk-scenery-type 't_magic_field_cyan          "magic field" s_magic_field_cyan)
(mk-scenery-type 't_magic_field_purple        "magic field" s_magic_field_purple)
(mk-scenery-type 't_magic_field_yellow        "magic field" s_magic_field_yellow)
(mk-scenery-type 't_magic_field_brown         "magic field" s_magic_field_brown)
(mk-scenery-type 't_magic_field_orange        "magic field" s_magic_field_orange)
(mk-scenery-type 't_magic_field_white         "magic field" s_magic_field_white)
(mk-scenery-type 't_magic_field_grey          "magic field" s_magic_field_grey)
(mk-scenery-type 't_magic_field_dark_grey     "magic field" s_magic_field_dark_grey)
(mk-scenery-type 't_magic_field_blue_2        "magic field" s_magic_field_blue_2)
(mk-scenery-type 't_magic_field_red_pulsed    "pulsing magic field" s_magic_field_red_pulsed)
(mk-scenery-type 't_magic_field_green_pulsed  "pulsing magic field" s_magic_field_green_pulsed)
(mk-scenery-type 't_magic_field_blue_pulsed   "pulsing magic field" s_magic_field_blue_pulsed)
(mk-scenery-type 't_magic_field_white_pulsed  "pulsing magic field" s_magic_field_white_pulsed)
(mk-scenery-type 't_magic_field_purple_pulsed "pulsing magic field" s_magic_field_purple_pulsed)
(mk-scenery-type 't_magic_field_brown_pulsed  "pulsing magic field" s_magic_field_brown_pulsed)
(mk-scenery-type 't_magic_field_blue_2_pulsed "pulsing magic field" s_magic_field_blue_2_pulsed)
;; Chromatic ball lightning (8 frames):
(mk-scenery-type 't_magic_field_chromatic_pulsed "pulsing magic field" s_magic_field_chromatic_pulsed)
;; Lightning discharges:
(mk-scenery-type 't_lightning_discharge_red   "lightning" s_lightning_discharge_red)
(mk-scenery-type 't_lightning_discharge_green "lightning" s_lightning_discharge_green)
(mk-scenery-type 't_lightning_discharge_blue  "lightning" s_lightning_discharge_blue)
(mk-scenery-type 't_lightning_discharge_black "lightning" s_lightning_discharge_black)
;; Lightning discharges in cloud:
(mk-scenery-type 't_lightning_cloud_red    "lightning cloud" s_lightning_cloud_blue_red)
(mk-scenery-type 't_lightning_cloud_green  "lightning cloud" s_lightning_cloud_blue_green)
(mk-scenery-type 't_lightning_cloud_blue   "lightning cloud" s_lightning_cloud_blue_blue)
(mk-scenery-type 't_lightning_cloud_purple "lightning cloud" s_lightning_cloud_blue_purple)
(mk-scenery-type 't_lightning_cloud_yellow "lightning cloud" s_lightning_cloud_blue_yellow)
(mk-scenery-type 't_lightning_cloud_orange "lightning cloud" s_lightning_cloud_blue_orange)
;; Magical spheres:
(mk-scenery-type 't_magic_sphere_red    "magical sphere" s_magic_sphere_red)
(mk-scenery-type 't_magic_sphere_green  "magical sphere" s_magic_sphere_green)
(mk-scenery-type 't_magic_sphere_blue   "magical sphere" s_magic_sphere_blue)
(mk-scenery-type 't_magic_sphere_yellow "magical sphere" s_magic_sphere_yellow)
(mk-scenery-type 't_magic_sphere_purple "magical sphere" s_magic_sphere_purple)
(mk-scenery-type 't_magic_sphere_cyan   "magical sphere" s_magic_sphere_cyan)
;; Magical spheres with light rays:
(mk-scenery-type 't_magic_sphere_rays_red    "magical sphere" s_magic_sphere_rays_red)
(mk-scenery-type 't_magic_sphere_rays_green  "magical sphere" s_magic_sphere_rays_green)
(mk-scenery-type 't_magic_sphere_rays_blue   "magical sphere" s_magic_sphere_rays_blue)
(mk-scenery-type 't_magic_sphere_rays_yellow "magical sphere" s_magic_sphere_rays_yellow)
(mk-scenery-type 't_magic_sphere_rays_purple "magical sphere" s_magic_sphere_rays_purple)
(mk-scenery-type 't_magic_sphere_rays_cyan   "magical sphere" s_magic_sphere_rays_cyan)
;; Arrows and Bolts and such in flight:
(mk-scenery-type 't_arrow_wood_red    "arrow" s_arrow_wood_red)
(mk-scenery-type 't_arrow_metal_red   "arrow" s_arrow_metal_red)
(mk-scenery-type 't_arrow_wood_green  "arrow" s_arrow_wood_green)
(mk-scenery-type 't_arrow_metal_green "arrow" s_arrow_metal_green)


;; ss_dg_wearable
(mk-scenery-type 't_helm_leather_1    "helm" s_helm_leather_1)
(mk-scenery-type 't_helm_leather_2    "helm" s_helm_leather_2)
(mk-scenery-type 't_helm_metal_1      "helm" s_helm_metal_1)
(mk-scenery-type 't_helm_metal_2      "helm" s_helm_metal_2)
(mk-scenery-type 't_helm_metal_3      "helm" s_helm_metal_3)
(mk-scenery-type 't_helm_gold_horned  "helm" s_helm_gold_horned)
(mk-scenery-type 't_helm_metal_horned "helm" s_helm_metal_horned)
(mk-scenery-type 't_hat_green_feather "helm" s_hat_green_feather)
(mk-scenery-type 't_crown_1           "helm" s_crown_1)
(mk-scenery-type 't_crown_2           "helm" s_crown_2)
(mk-scenery-type 't_crown_3           "helm" s_crown_3)
(mk-scenery-type 't_crown_4           "helm" s_crown_4)

(mk-scenery-type 't_amulet_1   "amulet" s_amulet_1)
(mk-scenery-type 't_amulet_2   "amulet" s_amulet_2)
(mk-scenery-type 't_amulet_3   "amulet" s_amulet_3)
(mk-scenery-type 't_amulet_4   "amulet" s_amulet_4)
(mk-scenery-type 't_amulet_5   "amulet" s_amulet_5)
(mk-scenery-type 't_amulet_6   "amulet" s_amulet_6)
(mk-scenery-type 't_amulet_7   "amulet" s_amulet_7)
(mk-scenery-type 't_amulet_8   "amulet" s_amulet_8)
(mk-scenery-type 't_amulet_9   "amulet" s_amulet_9)
(mk-scenery-type 't_amulet_10  "amulet" s_amulet_10)
(mk-scenery-type 't_amulet_11  "amulet" s_amulet_11)
(mk-scenery-type 't_amulet_12  "amulet" s_amulet_12)
(mk-scenery-type 't_amulet_13  "amulet" s_amulet_13)
(mk-scenery-type 't_amulet_14  "amulet" s_amulet_14)
(mk-scenery-type 't_amulet_15  "amulet" s_amulet_15)
(mk-scenery-type 't_amulet_16  "amulet" s_amulet_16)

(mk-scenery-type 't_ring_1   "ring" s_ring_1)
(mk-scenery-type 't_ring_2   "ring" s_ring_2)
(mk-scenery-type 't_ring_3   "ring" s_ring_3)
(mk-scenery-type 't_ring_4   "ring" s_ring_4)
(mk-scenery-type 't_ring_5   "ring" s_ring_5)
(mk-scenery-type 't_ring_6   "ring" s_ring_6)
(mk-scenery-type 't_ring_7   "ring" s_ring_7)
(mk-scenery-type 't_ring_8   "ring" s_ring_8)
(mk-scenery-type 't_ring_9   "ring" s_ring_9)
(mk-scenery-type 't_ring_10  "ring" s_ring_10)
(mk-scenery-type 't_ring_11  "ring" s_ring_11)
(mk-scenery-type 't_ring_12  "ring" s_ring_12)
(mk-scenery-type 't_ring_13  "ring" s_ring_13)
(mk-scenery-type 't_ring_14  "ring" s_ring_14)
(mk-scenery-type 't_ring_15  "ring" s_ring_15)
(mk-scenery-type 't_ring_16  "ring" s_ring_16)
(mk-scenery-type 't_ring_17  "ring" s_ring_17)
(mk-scenery-type 't_ring_18  "ring" s_ring_18)
(mk-scenery-type 't_ring_19  "ring" s_ring_19)
(mk-scenery-type 't_ring_20  "ring" s_ring_20)
(mk-scenery-type 't_ring_21  "ring" s_ring_21)
(mk-scenery-type 't_ring_22  "ring" s_ring_22)
(mk-scenery-type 't_ring_23  "ring" s_ring_23)
(mk-scenery-type 't_ring_24  "ring" s_ring_24)

(mk-scenery-type 't_cloak_1  "cloak" s_cloak_1)
(mk-scenery-type 't_cloak_2  "cloak" s_cloak_2)
(mk-scenery-type 't_cloak_3  "cloak" s_cloak_3)
(mk-scenery-type 't_cloak_4  "cloak" s_cloak_4)
(mk-scenery-type 't_cloak_5  "cloak" s_cloak_5)
(mk-scenery-type 't_cloak_6  "cloak" s_cloak_6)
(mk-scenery-type 't_cloak_7  "cloak" s_cloak_7)
(mk-scenery-type 't_cloak_8  "cloak" s_cloak_8)
(mk-scenery-type 't_cloak_9  "cloak" s_cloak_9)

(mk-scenery-type 't_robe_1    "robe" s_robe_1)
(mk-scenery-type 't_robe_2    "robe" s_robe_2)
(mk-scenery-type 't_robe_3    "robe" s_robe_3)
(mk-scenery-type 't_robe_4    "robe" s_robe_4)
(mk-scenery-type 't_robe_rags "robe" s_robe_rags)

(mk-scenery-type 't_gloves_green  "gloves" s_gloves_green)
(mk-scenery-type 't_gloves_tan    "gloves" s_gloves_tan)
(mk-scenery-type 't_gloves_brown  "gloves" s_gloves_brown)
(mk-scenery-type 't_gloves_grey   "gloves" s_gloves_grey)
(mk-scenery-type 't_gloves_yellow "gloves" s_gloves_yellow)
(mk-scenery-type 't_gloves_blue   "gloves" s_gloves_blue)

(mk-scenery-type 't_boots_green  "boots" s_boots_green)
(mk-scenery-type 't_boots_tan    "boots" s_boots_tan)
(mk-scenery-type 't_boots_brown  "boots" s_boots_brown)
(mk-scenery-type 't_boots_metal  "boots" s_boots_metal)
(mk-scenery-type 't_boots_yellow "boots" s_boots_yellow)

(mk-scenery-type 't_shield_pattern_1  "shield" s_shield_pattern_1)
(mk-scenery-type 't_shield_pattern_2  "shield" s_shield_pattern_2)
(mk-scenery-type 't_shield_pattern_3  "shield" s_shield_pattern_3)
(mk-scenery-type 't_shield_pattern_4  "shield" s_shield_pattern_4)
(mk-scenery-type 't_shield_pattern_5  "shield" s_shield_pattern_5)
(mk-scenery-type 't_shield_pattern_6  "shield" s_shield_pattern_6)
(mk-scenery-type 't_shield_pattern_7  "shield" s_shield_pattern_7)
(mk-scenery-type 't_shield_pattern_blank_gold "shield" s_shield_blank_gold)

(mk-scenery-type 't_shield_blank_wooden_1  "shield" s_shield_blank_wooden_1)
(mk-scenery-type 't_shield_blank_wooden_2  "shield" s_shield_blank_wooden_2)
(mk-scenery-type 't_shield_blank_round_1   "shield" s_shield_blank_round_1)
(mk-scenery-type 't_shield_blank_round_2   "shield" s_shield_blank_round_2)
(mk-scenery-type 't_shield_blank_round_3   "shield" s_shield_blank_round_3)
(mk-scenery-type 't_shield_blank_heater_1  "shield" s_shield_blank_heater_1)
(mk-scenery-type 't_shield_blank_heater_2  "shield" s_shield_blank_heater_2)

(mk-scenery-type 't_breastplate_dragon_1 "breastplate" s_breastplate_dragon_1)
(mk-scenery-type 't_breastplate_dragon_2 "breastplate" s_breastplate_dragon_2)
(mk-scenery-type 't_breastplate_dragon_3 "breastplate" s_breastplate_dragon_3)
(mk-scenery-type 't_breastplate_dragon_4 "breastplate" s_breastplate_dragon_4)
(mk-scenery-type 't_breastplate_dragon_5 "breastplate" s_breastplate_dragon_5)
(mk-scenery-type 't_breastplate_dragon_6 "breastplate" s_breastplate_dragon_6)

(mk-scenery-type 't_shield_symbol_crown        "shield" s_shield_symbol_crown)
(mk-scenery-type 't_shield_symbol_unicorn_1    "shield" s_shield_symbol_unicorn_1)
(mk-scenery-type 't_shield_symbol_unicorn_2    "shield" s_shield_symbol_unicorn_2)
(mk-scenery-type 't_shield_symbol_lion_rampant "shield" s_shield_symbol_lion_rampant)
(mk-scenery-type 't_shield_symbol_skull        "shield" s_shield_symbol_skull)

(mk-scenery-type 't_armor_leather_1  "leather armor" s_armor_leather_1)
(mk-scenery-type 't_armor_leather_2  "leather armor" s_armor_leather_2)
(mk-scenery-type 't_armor_leather_3  "leather armor" s_armor_leather_3)
(mk-scenery-type 't_armor_leather_4  "leather armor" s_armor_leather_4)
(mk-scenery-type 't_armor_leather_5  "leather armor" s_armor_leather_5)

(mk-scenery-type 't_armor_golden_1   "golden armor" s_armor_golden_1)
(mk-scenery-type 't_armor_green_1    "green armor"  s_armor_green_1)

(mk-scenery-type 't_armor_chain_1    "chain armor" s_armor_chain_1)
(mk-scenery-type 't_armor_chain_2    "chain armor" s_armor_chain_2)
(mk-scenery-type 't_armor_chain_3    "chain armor" s_armor_chain_3)
(mk-scenery-type 't_armor_chain_4    "chain armor" s_armor_chain_4)
(mk-scenery-type 't_armor_chain_5    "chain armor" s_armor_chain_5)
(mk-scenery-type 't_armor_chain_6    "chain armor" s_armor_chain_6)
(mk-scenery-type 't_armor_chain_7    "chain armor" s_armor_chain_7)
(mk-scenery-type 't_armor_chain_8    "chain armor" s_armor_chain_8)

(mk-scenery-type 't_armor_plate_1    "plate armor" s_armor_plate_1)
(mk-scenery-type 't_armor_plate_2    "plate armor" s_armor_plate_2)
(mk-scenery-type 't_armor_plate_3    "plate armor" s_armor_plate_3)
(mk-scenery-type 't_armor_plate_4    "plate armor" s_armor_plate_4)
(mk-scenery-type 't_armor_plate_5    "plate armor" s_armor_plate_5)

(mk-scenery-type 't_armor_dragon_1   "dragon armor" s_armor_dragon_1)
(mk-scenery-type 't_armor_dragon_2   "dragon armor" s_armor_dragon_2)
(mk-scenery-type 't_armor_dragon_3   "dragon armor" s_armor_dragon_3)
(mk-scenery-type 't_armor_dragon_4   "dragon armor" s_armor_dragon_4)
(mk-scenery-type 't_armor_dragon_5   "dragon armor" s_armor_dragon_5)
(mk-scenery-type 't_armor_dragon_6   "dragon armor" s_armor_dragon_6)
(mk-scenery-type 't_armor_dragon_7   "dragon armor" s_armor_dragon_7)
(mk-scenery-type 't_armor_dragon_8   "dragon armor" s_armor_dragon_8)
(mk-scenery-type 't_armor_dragon_9   "dragon armor" s_armor_dragon_9)
(mk-scenery-type 't_armor_dragon_10  "dragon armor" s_armor_dragon_10)
(mk-scenery-type 't_armor_dragon_11  "dragon armor" s_armor_dragon_11)
(mk-scenery-type 't_armor_dragon_12  "dragon armor" s_armor_dragon_12)
(mk-scenery-type 't_armor_dragon_13  "dragon armor" s_armor_dragon_13)
(mk-scenery-type 't_armor_dragon_14  "dragon armor" s_armor_dragon_14)
(mk-scenery-type 't_armor_dragon_15  "dragon armor" s_armor_dragon_15)
(mk-scenery-type 't_armor_dragon_16  "dragon armor" s_armor_dragon_16)

