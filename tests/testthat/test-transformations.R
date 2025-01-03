# df_sleap_single |>
#   # translate_coords(to_keypoint = "centre") |>
#   # rotate_coords(alignment_points = c("centre", "snout"),
#   #               align_perpendicular = FALSE)
#   add_centroid(include_keypoints = c("left_ear", "right_ear")) |>
#   transform_to_egocentric(
#     # alignment_points = c("centre", "snout"),
#     # align_perpendicular = FALSE,
#     alignment_points = c("left_ear", "right_ear"),
#     align_perpendicular = TRUE,
#     to_keypoint = "centroid") |>
#   # filter(keypoint == "centroid") |>
#   filter(time < 500 & time > 400) |>
#   ggplot(aes(x,y, colour = keypoint), alpha = 0.3) +
#   # ggplot(aes(x,y, colour = time, group = keypoint)) +
#   # scale_colour_viridis_c() +
#   geom_path(alpha = 0.3)
#
#
# df_dlc_multi |>
#   # translate_coords(to_keypoint = "centre") |>
#   # rotate_coords(alignment_points = c("centre", "snout"),
#   #               align_perpendicular = FALSE)
#   add_centroid(include_keypoints = c("leftear", "rightear")) |>
#   transform_to_egocentric(
#     alignment_points = c("leftear", "rightear"),
#     align_perpendicular = TRUE,
#     # alignment_points = c("left_ear", "right_ear"),
#     # align_perpendicular = TRUE,
#     to_keypoint = "centroid") |>
#   # filter(keypoint == "centroid") |>
#   # filter(time < 500 & time > 400) |>
#   # ggplot(aes(x,y, colour = keypoint), alpha = 0.3) +
#   ggplot(aes(x,y, colour = time, group = keypoint)) +
#   scale_colour_viridis_c() +
#   geom_path(alpha = 0.3) +
#   facet_grid(cols = vars(individual))
