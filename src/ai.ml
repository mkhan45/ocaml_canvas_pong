open Core
open Util

open Lens.Infix
(* open Js_of_ocaml.Firebug *)

exception ZeroVel of unit

let (%.) = Float.(%)

let top_ai = ref false
let bottom_ai = ref false

let calculate_landing_pos (ball: Rect.t) : float =
    let target_y = match Float.robust_sign ball.vel.y with
    | Sign.Pos -> 780.0
    | Sign.Neg -> 20.0
    | Sign.Zero -> raise (ZeroVel ())
    in
    let dy = target_y -. ball.pos.y in
    let dt = dy /. ball.vel.y in
    let dx = dt *. ball.vel.x in
    let unwrapped_final_ball_pos = ball.pos.x +. dx in
    let num_bounces = Float.iround_down_exn (unwrapped_final_ball_pos /. 800.0) in
    let wrapped_pos = unwrapped_final_ball_pos %. 800.0 in
    let extra_wrap = if num_bounces % 2 = 0 then wrapped_pos else 800.0 -. wrapped_pos in
    extra_wrap

let ai_moved paddle ~ball ~paddle_speed =
    let open Rect in
    let dy_sign = Float.robust_sign (paddle.pos.y -. ball.pos.y)
    and dydt_sign = Float.robust_sign ball.vel.y in
    let target_pos = if phys_equal dy_sign dydt_sign 
                        then calculate_landing_pos ball 
                        else paddle.pos.x +. (paddle.bounds.x /. 2.0)
    in
    let dx = target_pos -. (paddle.pos.x +. (paddle.bounds.x /. 2.0)) in
    let dx_signum = if dx >. 0.0 then 1.0 else -1.0 in
    if (Float.abs dx) >. 10.0 
        then paddle |> ((Rect.vel |-- Vector.x) ^= paddle_speed *. dx_signum)
        else paddle |> ((Rect.vel |-- Vector.x) ^= 0.0)
