;; TODO: entrance to Sprite Gallery 1 should be a portal, not a subplace
;; TODO: We need 32x32 number (and roman numeral II, III, ...) sprites


(kern-mk-map 
 'm_sprite_gallery_1 19 40 pal_expanded
  ;                               1  1  1  1  1  1  1  1  1
  ; 0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5  6  7  8
 (list
  "xx [ .S .P .R .I .T .E  @  @  .G .A .L .L .E .R .Y ]  xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .M .I .S .S .I .L .E .S .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx "
  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  ))
  ;                               1  1  1  1  1  1  1  1  1
  ; 0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5  6  7  8




(kern-mk-place 'p_sprite_gallery_1 ; tag
               "Sprite Gallery 1"  ; name
               s_moongate_full     ; sprite
               m_sprite_gallery_1  ; map
               #f                  ; wraps
               #f                  ; underground
               #f                  ; large-scale (wilderness)
               #f                  ; tmp combat place
               nil ; subplaces
               nil ; neighbors
               (list
                (list (mk-perm-gate 'p_terrain_test 21 2)  9  1)

                ;; Lightning bolts:
                (list (kern-mk-obj t_lightning_bolt_red       1)  1  2)
                (list (kern-mk-obj t_lightning_bolt_green     1)  1  3)
                (list (kern-mk-obj t_lightning_bolt_blue      1)  1  4)
                (list (kern-mk-obj t_lightning_bolt_cyan      1)  1  5)
                (list (kern-mk-obj t_lightning_bolt_purple    1)  1  6)
                (list (kern-mk-obj t_lightning_bolt_yellow    1)  1  7)
                (list (kern-mk-obj t_lightning_bolt_brown     1)  1  8)
                (list (kern-mk-obj t_lightning_bolt_orange    1)  1  9)
                (list (kern-mk-obj t_lightning_bolt_white     1)  1 10)
                (list (kern-mk-obj t_lightning_bolt_grey      1)  1 11)
                (list (kern-mk-obj t_lightning_bolt_dark_grey 1)  1 12)
                (list (kern-mk-obj t_lightning_bolt_chromatic 1)  1 13)

                ;; Magic ball (fireball core or similar):
                (list (kern-mk-obj t_fireball_red       1)  3  2)
                (list (kern-mk-obj t_fireball_green     1)  3  3)
                (list (kern-mk-obj t_fireball_blue      1)  3  4)
                (list (kern-mk-obj t_fireball_cyan      1)  3  5)
                (list (kern-mk-obj t_fireball_purple    1)  3  6)
                (list (kern-mk-obj t_fireball_yellow    1)  3  7)
                (list (kern-mk-obj t_fireball_brown     1)  3  8)
                (list (kern-mk-obj t_fireball_orange    1)  3  9)
                (list (kern-mk-obj t_fireball_white     1)  3 10)
                (list (kern-mk-obj t_fireball_grey      1)  3 11)
                (list (kern-mk-obj t_fireball_dark_grey 1)  3 12)
                (list (kern-mk-obj t_fireball_chromatic 1)  3 13)

                ;; Magic burst/bloom/aura/field (5 ball lightnings in one tile):
                (list (kern-mk-obj t_magic_field_red       1)  5  2)
                (list (kern-mk-obj t_magic_field_green     1)  5  3)
                (list (kern-mk-obj t_magic_field_blue      1)  5  4)
                (list (kern-mk-obj t_magic_field_cyan      1)  5  5)
                (list (kern-mk-obj t_magic_field_purple    1)  5  6)
                (list (kern-mk-obj t_magic_field_yellow    1)  5  7)
                (list (kern-mk-obj t_magic_field_brown     1)  5  8)
                (list (kern-mk-obj t_magic_field_orange    1)  5  9)
                (list (kern-mk-obj t_magic_field_white     1)  5 10)
                (list (kern-mk-obj t_magic_field_grey      1)  5 11)
                (list (kern-mk-obj t_magic_field_dark_grey 1)  5 12)
                (list (kern-mk-obj t_magic_field_blue_2    1)  5 13)

                (list (kern-mk-obj t_magic_field_red_pulsed       1)  7  2)
                (list (kern-mk-obj t_magic_field_green_pulsed     1)  7  3)
                (list (kern-mk-obj t_magic_field_blue_pulsed      1)  7  4)
                (list (kern-mk-obj t_magic_field_white_pulsed     1)  7  5)

                (list (kern-mk-obj t_magic_field_purple_pulsed    1)  7  7)
                (list (kern-mk-obj t_magic_field_brown_pulsed     1)  7  8)
                (list (kern-mk-obj t_magic_field_blue_2_pulsed    1)  7  9)
                (list (kern-mk-obj t_magic_field_chromatic_pulsed 1)  7 10)

                ;; Lightning discharges:
                (list (kern-mk-obj t_lightning_discharge_red   1)  9  2)
                (list (kern-mk-obj t_lightning_discharge_green 1)  9  3)
                (list (kern-mk-obj t_lightning_discharge_blue  1)  9  4)
                (list (kern-mk-obj t_lightning_discharge_black 1)  9  5)

                ;; Lightning discharges in cloud:
                (list (kern-mk-obj t_lightning_cloud_red    1)  9  7)
                (list (kern-mk-obj t_lightning_cloud_green  1)  9  8)
                (list (kern-mk-obj t_lightning_cloud_blue   1)  9  9)
                (list (kern-mk-obj t_lightning_cloud_purple 1)  9 10)
                (list (kern-mk-obj t_lightning_cloud_yellow 1)  9 11)
                (list (kern-mk-obj t_lightning_cloud_orange 1)  9 12)

                ;; Magical spheres:
                (list (kern-mk-obj t_magic_sphere_red    1) 11  2)
                (list (kern-mk-obj t_magic_sphere_green  1) 11  3)
                (list (kern-mk-obj t_magic_sphere_blue   1) 11  4)
                (list (kern-mk-obj t_magic_sphere_yellow 1) 11  5)
                (list (kern-mk-obj t_magic_sphere_purple 1) 11  6)
                (list (kern-mk-obj t_magic_sphere_cyan   1) 11  7)

                ;; Magical spheres with light rays:
                (list (kern-mk-obj t_magic_sphere_rays_red    1) 11  9)
                (list (kern-mk-obj t_magic_sphere_rays_green  1) 11 10)
                (list (kern-mk-obj t_magic_sphere_rays_blue   1) 11 11)
                (list (kern-mk-obj t_magic_sphere_rays_yellow 1) 11 12)
                (list (kern-mk-obj t_magic_sphere_rays_purple 1) 11 13)
                (list (kern-mk-obj t_magic_sphere_rays_cyan   1) 11 14)

                ;; Arrows and Bolts and such in flight:
                (list (kern-mk-obj t_arrow_wood_red    1) 13  2)
                (list (kern-mk-obj t_arrow_metal_red   1) 13  3)
                (list (kern-mk-obj t_arrow_wood_green  1) 13  4)
                (list (kern-mk-obj t_arrow_metal_green 1) 13  5)




(list (kern-mk-obj t_helm_leather_1    1)  1  19)
(list (kern-mk-obj t_helm_leather_2    1)  2  19)
(list (kern-mk-obj t_helm_metal_1      1)  3  19)
(list (kern-mk-obj t_helm_metal_2      1)  4  19)
(list (kern-mk-obj t_helm_metal_3      1)  5  19)
(list (kern-mk-obj t_helm_gold_horned  1)  6  19)
(list (kern-mk-obj t_helm_metal_horned 1)  7  19)
(list (kern-mk-obj t_hat_green_feather 1)  8  19)
(list (kern-mk-obj t_crown_1           1)  9  19)
(list (kern-mk-obj t_crown_2           1) 10  19)
(list (kern-mk-obj t_crown_3           1) 11  19)
(list (kern-mk-obj t_crown_4           1) 12  19)

(list (kern-mk-obj t_amulet_1  1)  1  20)
(list (kern-mk-obj t_amulet_2  1)  2  20)
(list (kern-mk-obj t_amulet_3  1)  3  20)
(list (kern-mk-obj t_amulet_4  1)  4  20)
(list (kern-mk-obj t_amulet_5  1)  5  20)
(list (kern-mk-obj t_amulet_6  1)  6  20)
(list (kern-mk-obj t_amulet_7  1)  7  20)
(list (kern-mk-obj t_amulet_8  1)  8  20)
(list (kern-mk-obj t_amulet_9  1)  9  20)
(list (kern-mk-obj t_amulet_10 1) 10  20)
(list (kern-mk-obj t_amulet_11 1) 11  20)
(list (kern-mk-obj t_amulet_12 1) 12  20)
(list (kern-mk-obj t_amulet_13 1) 13  20)
(list (kern-mk-obj t_amulet_14 1) 14  20)
(list (kern-mk-obj t_amulet_15 1) 15  20)
(list (kern-mk-obj t_amulet_16 1) 16  20)

(list (kern-mk-obj t_ring_1  1)  1  21)
(list (kern-mk-obj t_ring_2  1)  2  21)
(list (kern-mk-obj t_ring_3  1)  3  21)
(list (kern-mk-obj t_ring_4  1)  4  21)
(list (kern-mk-obj t_ring_5  1)  5  21)
(list (kern-mk-obj t_ring_6  1)  6  21)
(list (kern-mk-obj t_ring_7  1)  7  21)
(list (kern-mk-obj t_ring_8  1)  8  21)
(list (kern-mk-obj t_ring_9  1)  9  21)
(list (kern-mk-obj t_ring_10 1) 10  21)
(list (kern-mk-obj t_ring_11 1) 11  21)
(list (kern-mk-obj t_ring_12 1) 12  21)
(list (kern-mk-obj t_ring_13 1) 13  21)
(list (kern-mk-obj t_ring_14 1) 14  21)
(list (kern-mk-obj t_ring_15 1) 15  21)
(list (kern-mk-obj t_ring_16 1) 16  21)
(list (kern-mk-obj t_ring_17 1) 17  21)

(list (kern-mk-obj t_ring_18 1)  1  22)
(list (kern-mk-obj t_ring_21 1)  2  22)
(list (kern-mk-obj t_ring_20 1)  3  22)
(list (kern-mk-obj t_ring_21 1)  4  22)
(list (kern-mk-obj t_ring_22 1)  5  22)
(list (kern-mk-obj t_ring_23 1)  6  22)
(list (kern-mk-obj t_ring_24 1)  7  22)

(list (kern-mk-obj t_cloak_1 1)  1  23)
(list (kern-mk-obj t_cloak_2 1)  2  23)
(list (kern-mk-obj t_cloak_3 1)  3  23)
(list (kern-mk-obj t_cloak_4 1)  4  23)
(list (kern-mk-obj t_cloak_5 1)  5  23)
(list (kern-mk-obj t_cloak_6 1)  6  23)
(list (kern-mk-obj t_cloak_7 1)  7  23)
(list (kern-mk-obj t_cloak_8 1)  8  23)
(list (kern-mk-obj t_cloak_9 1)  9  23)

(list (kern-mk-obj t_robe_1    1) 11  23)
(list (kern-mk-obj t_robe_2    1) 12  23)
(list (kern-mk-obj t_robe_3    1) 13  23)
(list (kern-mk-obj t_robe_4    1) 14  23)
(list (kern-mk-obj t_robe_rags 1) 15  23)

(list (kern-mk-obj t_gloves_green  1)  1  24)
(list (kern-mk-obj t_gloves_tan    1)  2  24)
(list (kern-mk-obj t_gloves_brown  1)  3  24)
(list (kern-mk-obj t_gloves_grey   1)  4  24)
(list (kern-mk-obj t_gloves_yellow 1)  5  24)
(list (kern-mk-obj t_gloves_blue   1)  6  24)

(list (kern-mk-obj t_boots_green   1)  8  24)
(list (kern-mk-obj t_boots_tan     1)  9  24)
(list (kern-mk-obj t_boots_brown   1) 10  24)
(list (kern-mk-obj t_boots_metal   1) 11  24)
(list (kern-mk-obj t_boots_yellow  1) 12  24)

(list (kern-mk-obj t_shield_pattern_1 1)  1  25)
(list (kern-mk-obj t_shield_pattern_2 1)  2  25)
(list (kern-mk-obj t_shield_pattern_3 1)  3  25)
(list (kern-mk-obj t_shield_pattern_4 1)  4  25)
(list (kern-mk-obj t_shield_pattern_5 1)  5  25)
(list (kern-mk-obj t_shield_pattern_6 1)  6  25)
(list (kern-mk-obj t_shield_pattern_7 1)  7  25)
(list (kern-mk-obj t_shield_pattern_blank_gold 1)  8  25)

(list (kern-mk-obj t_shield_blank_wooden_1 1) 10  25)
(list (kern-mk-obj t_shield_blank_wooden_2 1) 11  25)
(list (kern-mk-obj t_shield_blank_round_1  1) 12  25)
(list (kern-mk-obj t_shield_blank_round_2  1) 13  25)
(list (kern-mk-obj t_shield_blank_round_3  1) 14  25)
(list (kern-mk-obj t_shield_blank_heater_1 1) 15  25)
(list (kern-mk-obj t_shield_blank_heater_2 1) 16  25)

(list (kern-mk-obj t_breastplate_dragon_1  1)  1  26)
(list (kern-mk-obj t_breastplate_dragon_2  1)  2  26)
(list (kern-mk-obj t_breastplate_dragon_3  1)  3  26)
(list (kern-mk-obj t_breastplate_dragon_4  1)  4  26)
(list (kern-mk-obj t_breastplate_dragon_5  1)  5  26)
(list (kern-mk-obj t_breastplate_dragon_6  1)  6  26)

(list (kern-mk-obj t_shield_symbol_crown        1)  7  26)
(list (kern-mk-obj t_shield_symbol_unicorn_1    1)  8  26)
(list (kern-mk-obj t_shield_symbol_unicorn_2    1)  9  26)
(list (kern-mk-obj t_shield_symbol_lion_rampant 1) 10  26)
(list (kern-mk-obj t_shield_symbol_skull        1) 11  26)

(list (kern-mk-obj t_armor_leather_1 1)  1  27)
(list (kern-mk-obj t_armor_leather_2 1)  2  27)
(list (kern-mk-obj t_armor_leather_3 1)  3  27)
(list (kern-mk-obj t_armor_leather_4 1)  4  27)
(list (kern-mk-obj t_armor_leather_5 1)  5  27)

(list (kern-mk-obj t_armor_golden_1  1)  7  27)
(list (kern-mk-obj t_armor_green_1   1)  8  27)

(list (kern-mk-obj t_armor_chain_1   1) 10  27)
(list (kern-mk-obj t_armor_chain_2   1) 11  27)
(list (kern-mk-obj t_armor_chain_3   1) 12  27)
(list (kern-mk-obj t_armor_chain_4   1) 13  27)
(list (kern-mk-obj t_armor_chain_5   1) 14  27)
(list (kern-mk-obj t_armor_chain_6   1) 15  27)
(list (kern-mk-obj t_armor_chain_7   1) 16  27)
(list (kern-mk-obj t_armor_chain_8   1) 17  27)

(list (kern-mk-obj t_armor_plate_1   1)  1  28)
(list (kern-mk-obj t_armor_plate_2   1)  2  28)
(list (kern-mk-obj t_armor_plate_3   1)  3  28)
(list (kern-mk-obj t_armor_plate_4   1)  4  28)
(list (kern-mk-obj t_armor_plate_5   1)  5  28)

(list (kern-mk-obj t_armor_dragon_1  1)  1  29)
(list (kern-mk-obj t_armor_dragon_2  1)  2  29)
(list (kern-mk-obj t_armor_dragon_3  1)  3  29)
(list (kern-mk-obj t_armor_dragon_4  1)  4  29)
(list (kern-mk-obj t_armor_dragon_5  1)  5  29)
(list (kern-mk-obj t_armor_dragon_6  1)  6  29)
(list (kern-mk-obj t_armor_dragon_7  1)  7  29)
(list (kern-mk-obj t_armor_dragon_8  1)  8  29)
(list (kern-mk-obj t_armor_dragon_9  1)  9  29)
(list (kern-mk-obj t_armor_dragon_10 1) 10  29)
(list (kern-mk-obj t_armor_dragon_11 1) 11  29)
(list (kern-mk-obj t_armor_dragon_12 1) 12  29)
(list (kern-mk-obj t_armor_dragon_13 1) 13  29)
(list (kern-mk-obj t_armor_dragon_14 1) 14  29)
(list (kern-mk-obj t_armor_dragon_15 1) 15  29)
(list (kern-mk-obj t_armor_dragon_16 1) 16  29)  ;; segfault


                
                ) ; objects
               nil ; hooks
               )


(kern-mk-map 
 'm_terrain_test 24 32 pal_expanded
  ;                               1  1  1  1  1  1  1  1  1  1  2  2  2  2 
  ; 0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5  6  7  8  9  0  1  2  3 
 (list
  "xx xx xx .. .. xx xx xx .. .. xx .. .. .. xx .. .. xx xx xx .. .. xx xx"
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx .. .A .B .C .D .E .F .G .H .. .I .J .K .L .M .N .O .P .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. ,A ,B ,C ,D ,E ,F ,G ,H .. ,I ,J ,K ,L ,M ,N ,O ,P .. .. .. .. .."
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx .. .Q .R .S .T .U .V .W .X .. .Y .Z  [  @  @  @  @ ]  .. .. .. .. xx"
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. ,Q ,R ,S ,T ,U ,V ,W ,X .. ,Y ,Z ;T ;E ;N ;A ;S ;D .. .. .. .. .."
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx .. __ __ __ .. %% %% %% .. {{ {{ {{ .. .. .. .. .. .. .. .. .. .. xx"
  ".. .. __ __ .. .. %% %% .. .. {{ {{ .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. -- -- -- .. tt tt tt .. ^^ ^^ ^^ .. .. .. .. .. .. .. .. .. .. .."
  ".. .. -- -- .. .. tt tt .. .. ^^ ^^ .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. ~~ ~~ ~~ .. || || || .. !  !  !  .. .. .. .. .. .. .. .. .. .. .."
  ".. .. ~~ ~~ .. .. || || .. .. !  !  .. .. .. .. .. .. .. .. .. .. .. .."
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  ".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .."
  "xx .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. xx"
  "xx xx xx .. .. xx xx xx .. .. xx .. .. .. xx .. .. xx xx xx .. .. xx xx"
  ))
  ;                               1  1  1  1  1  1  1  1  1  1  2  2  2  2 
  ; 0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5  6  7  8  9  0  1  2  3 


(kern-mk-place 'p_terrain_test    ; tag
               "Terrain Test"     ; name
               s_dungeon          ; sprite
               m_terrain_test     ; map
               #f                 ; wraps
               #f                 ; underground
               #f                 ; large-scale (wilderness)
               #f                 ; tmp combat place
               nil ; subplaces

;; BUG: A subplace of this small-scale place puts an object at the x,y
;;      but stepping on, or (E)nter, does not enter that place.
;               (list
;                (list p_sprite_gallery_1  21  2)  ; TODO: entrance should be a portal, not subplace
;                ) ; subplaces

               nil ; neighbors
               (list
                (list (mk-perm-gate 'p_sprite_gallery_1  9  1) 21  2)

                ) ; objects
               nil ; hooks
               )















;; eof
