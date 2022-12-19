open Core
open Js_of_ocaml

open Pixi_bindings
open Util
open Util.Vector.Ops

open Lens.Infix

let paddle_bounds = Vector.{x = 110.0; y = 10.0}
let init_paddle_speed = 4.5

let (graphics, app) = pixi_setup ()
let ball_speed_label = Dom_html.getElementById "ball-speed"

let _ = 
    let top_ai_btn = Dom_html.getElementById "top-ai"
    and btm_ai_btn = Dom_html.getElementById "btm-ai"
    in
    top_ai_btn##.onclick := Dom_html.handler (fun _ -> Ai.top_ai := not !Ai.top_ai; Js._false);
    btm_ai_btn##.onclick := Dom_html.handler (fun _ -> Ai.bottom_ai := not !Ai.bottom_ai; Js._false);

module Rect = struct
    include Util.Rect

    let draw { pos; bounds; _ } = graphics##drawRect pos.x pos.y bounds.x bounds.y
end

(* Makes a sane random velocity *)
let random_vel () =
    let random_x = Random.float 5.0
    and random_y = Random.float 2.5 +. 2.5
    and invert_x = if Random.bool () then -1.0 else 1.0
    and invert_y = if Random.bool () then -1.0 else 1.0
    in Vector.{x = random_x *. invert_x; y = random_y *. invert_y}

module GameState = struct
    type t = { top_paddle: Rect.t
             ; bottom_paddle: Rect.t
             ; ball: Rect.t
             ; paddle_speed: float 
             ; paused: bool } [@@deriving lens] 

    let draw game =
        let _ = graphics##clear in
        let _ = graphics##beginFill 0xffffff in
        List.iter ~f:Rect.draw [game.top_paddle; game.bottom_paddle; game.ball]

    let updated { top_paddle; bottom_paddle; ball; paddle_speed; paused } =
        let ball_hit_paddle = 
            (Rect.overlaps top_paddle ball && ball.vel.y <. 0.0) 
         || (Rect.overlaps bottom_paddle ball && ball.vel.y >. 0.0)
        and point_scored = Rect.top ball <. 0.0 || Rect.bottom ball >. 800.0 
        in
        { top_paddle = top_paddle
          |> Rect.integrate
          |> (fun paddle -> match !Input.input_state with
          | { left_pressed = true; right_pressed = false; _ } -> 
                paddle |> ((Rect.vel |-- Vector.x) ^= -1.0 *. paddle_speed)

          | { left_pressed = false; right_pressed = true; _ } -> 
                paddle |> ((Rect.vel |-- Vector.x) ^= paddle_speed)

          | _ -> paddle |> (Rect.vel ^= Vector.zero))
          |> (fun paddle -> if !Ai.top_ai then Ai.ai_moved ~ball ~paddle_speed paddle else paddle)
          |> (fun new_paddle -> if paused then top_paddle else new_paddle)

        ; bottom_paddle = bottom_paddle
          |> Rect.integrate
          |> (fun paddle -> match !Input.input_state with
              | { a_pressed = true; d_pressed = false; _ } -> 
                      paddle |> ((Rect.vel |-- Vector.x) ^= -1.0 *. paddle_speed)

              | { a_pressed = false; d_pressed = true; _ } -> 
                      paddle |> ((Rect.vel |-- Vector.x) ^= paddle_speed)

              | _ -> paddle |> (Rect.vel ^= Vector.zero))
          |> (fun paddle -> if !Ai.bottom_ai then Ai.ai_moved ~ball ~paddle_speed paddle else paddle)
          |> (fun new_paddle -> if paused then bottom_paddle else new_paddle)

        ; ball = ball
          |> Rect.integrate
          |> (fun ball -> if Rect.left ball <. 0.0 || Rect.right ball >. 800.0 
                          then Lens.modify (Rect.vel |-- Vector.x) (( *. ) (-1.0)) ball
                          else ball)

          |> (fun ball -> if ball_hit_paddle
                          then Lens.modify Rect.vel (fun vel -> { vel with y = vel.y *. -1.0 } <*> 1.1) ball
                          else ball)

          |> (fun ball -> if point_scored
                          then { ball with pos = { x = 400.0; y = 400.0 }; vel = random_vel () }
                          else ball)
          |> (fun new_ball -> if paused then ball else new_ball)

        ; paddle_speed = 
            if ball_hit_paddle then paddle_speed *. 1.1 
            else if point_scored then init_paddle_speed
            else paddle_speed 
        ; paused = if (!Input.input_state).space_pressed then not paused else paused }
end

(* Initial Game State *)
let game_state = ref GameState.{
    top_paddle = Rect.{ 
        pos = Vector.{x = 400.0 -. paddle_bounds.x /. 2.0; y = 10.0}; 
        bounds = paddle_bounds;
        vel = Vector.zero
    };
    bottom_paddle = Rect.{
        pos = Vector.{x = 400.0 -. paddle_bounds.x /. 2.0; y = 780.0}; 
        bounds = paddle_bounds;
        vel = Vector.zero
    };
    ball = Rect.{
        pos = Vector.{x = 400.0; y = 400.0};
        bounds = Vector.{x = 12.0; y = 12.0};
        vel = random_vel ();
    };
    paddle_speed = init_paddle_speed;
    paused = false;
}

let _ = app##.ticker##add (fun _delta -> begin
    GameState.draw !game_state;
    game_state := GameState.updated !game_state;
    Input.input_state := { !Input.input_state with space_pressed = false }; (* Since we only care about a single press*)
    ball_speed_label##.innerHTML := !game_state.ball.vel |> Vector.magnitude |> Float.to_string_hum |> Js.string;
end)
