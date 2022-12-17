open Core

open Js_of_ocaml
open Pixi_bindings
open Util
open Util.Vector.Ops

let (>.) = Float.(>)
let (<.) = Float.(<)

let _ = Random.self_init

(* Initializes PIXI graphics for drawing primitives *)
let graphics = 
    let graphics = new%js graphics () in
    let _ = graphics##beginFill 0xffffff
    in graphics

(* Initializes PIXI application *)
let app = 
    let app = new%js application object%js val width = 800.0; val height = 800.0 end in
    let _ = Dom.appendChild Dom_html.document##.body app##.view
    and _ = app##.stage##addChild(graphics) in
    app

let _ = Js.Unsafe.global##addEventListener "keyup" Input.handle_keyup
let _ = Js.Unsafe.global##addEventListener "keydown" Input.handle_keydown

module Rect = struct
    include Util.Rect

    let draw { pos; bounds; _ } = graphics##drawRect pos.x pos.y bounds.x bounds.y
end

(* Makes a sane random velocity *)
let random_vel () =
    let rec attempt () = match Random.float 5.0 -. 2.5 with
    | r when Float.abs r <. 2.0 -> attempt ()
    | r -> r
    in Vector.{ x = attempt (); y = attempt () } 

let paddle_bounds = Vector.{x = 100.0; y = 10.0}
let init_paddle_speed = 4.5

module GameState = struct
    type t = { top_paddle: Rect.t; bottom_paddle: Rect.t; ball: Rect.t; paddle_speed: float }

    let draw game =
        let _ = graphics##clear in
        let _ = graphics##beginFill 0xffffff in
        let _ = Rect.draw game.top_paddle
        and _ = Rect.draw game.bottom_paddle
        and _ = Rect.draw game.ball in
        ()

    let updated { top_paddle; bottom_paddle; ball; paddle_speed } =
        let ball_hit_paddle = 
            (Rect.overlaps top_paddle ball && ball.vel.y <. 0.0) 
         || (Rect.overlaps bottom_paddle ball && ball.vel.y >. 0.0)
        and point_scored = Rect.top ball <. 0.0 || Rect.bottom ball >. 800.0 in
        { top_paddle = top_paddle
          |> Rect.integrate
          |> (fun paddle -> match !Input.input_state with
          | { left_pressed = true; right_pressed = false; _ } -> 
                { paddle with vel = { paddle.vel with x = -1.0 *. paddle_speed } }
          | { left_pressed = false; right_pressed = true; _ } -> 
                { paddle with vel = { paddle.vel with x = paddle_speed } }
          | _ -> { paddle with vel = Vector.zero })
        ; bottom_paddle = bottom_paddle
          |> Rect.integrate
          |> (fun paddle -> match !Input.input_state with
              | { a_pressed = true; d_pressed = false; _ } -> 
                      { paddle with vel = { paddle.vel with x = -1.0 *. paddle_speed } }
              | { a_pressed = false; d_pressed = true; _ } -> 
                      { paddle with vel = { paddle.vel with x = paddle_speed } }
              | _ -> { paddle with vel = Vector.zero })
        ; ball = ball
          |> Rect.integrate
          |> (fun ball -> if Rect.left ball <. 0.0 || Rect.right ball >. 800.0 
                          then { ball with vel = { ball.vel with x = ball.vel.x *. -1.0 } }
                          else ball)
          |> (fun ball -> if ball_hit_paddle
                          then { ball with vel = { ball.vel with y = ball.vel.y *. -1.0 } <*> 1.1 }
                          else ball)
          |> (fun ball -> if point_scored
                          then { ball with pos = { x = 400.0; y = 400.0 }; vel = random_vel () }
                          else ball)
        ; paddle_speed = 
            if ball_hit_paddle then paddle_speed *. 1.1 
            else if point_scored then init_paddle_speed
            else paddle_speed }
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
}

let update _delta =
    GameState.draw !game_state;
    game_state := GameState.updated !game_state

let () =
    Js.Unsafe.global##.pong := object%js
        val app = app
        val graphics = graphics
    end;
    let _ = app##.ticker##add update in
    ()
