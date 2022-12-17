open Core

open Pixi_bindings
open Js_of_ocaml

let (>.) = Float.(>)
let (<.) = Float.(<)

let _ = Random.self_init

let graphics = 
    let graphics = new%js graphics () in
    let _ = graphics##beginFill 0xffffff
    in graphics

let app = 
    let app = new%js application object%js val width = 800.0; val height = 800.0 end in
    let _ = Dom.appendChild Dom_html.document##.body app##.view
    and _ = app##.stage##addChild(graphics) in
    app

type input = { left_pressed: bool
             ; right_pressed: bool
             ; a_pressed: bool
             ; d_pressed: bool }

let input_state = ref { left_pressed = false
                      ; right_pressed = false
                      ; a_pressed = false
                      ; d_pressed = false }

let handle_keyup ev = match (Js.to_string ev##.key) with
| "a" -> input_state := { !input_state with a_pressed = false }
| "d" -> input_state := { !input_state with d_pressed = false }
| "ArrowLeft" -> input_state := { !input_state with left_pressed = false }
| "ArrowRight" -> input_state := { !input_state with right_pressed = false }
| _ -> ()

let handle_keydown ev = match (Js.to_string ev##.key) with
| "a" -> input_state := { !input_state with a_pressed = true }
| "d" -> input_state := { !input_state with d_pressed = true }
| "ArrowLeft" -> input_state := { !input_state with left_pressed = true }
| "ArrowRight" -> input_state := { !input_state with right_pressed = true }
| _ -> ()

let _ = Js.Unsafe.global##addEventListener "keyup" handle_keyup
let _ = Js.Unsafe.global##addEventListener "keydown" handle_keydown

module Vector = struct
    type t = {x: float; y: float}

    module Ops = struct
        let (<+>) a b = { x = a.x +. b.x; y = a.y +. b.y }
        let (<*>) a s = { x = a.x *. s; y = a.y *. s }
    end
end
open Vector.Ops
let random_vel () =
    let rec attempt () = match Random.float 5.0 -. 2.5 with
    | r when Float.abs r <. 2.0 -> attempt ()
    | r -> r
    in Vector.{ x = attempt (); y = attempt () } 

module Rect = struct
    type t = {pos: Vector.t; bounds: Vector.t; vel: Vector.t}

    let top rect = rect.pos.y
    let bottom rect = rect.pos.y +. rect.bounds.y

    let left rect = rect.pos.x
    let right rect = rect.pos.x +. rect.bounds.x

    let overlap a b = List.for_all ~f:(phys_equal false) [
        left a >. right b;
        right a <. left b;
        top a >. bottom b;
        bottom a <. top b;
    ]

    let draw { pos; bounds; _ } = graphics##drawRect pos.x pos.y bounds.x bounds.y
    let integrate { pos; bounds; vel } = { pos = pos <+> vel; bounds; vel }
end

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
        { top_paddle = top_paddle
                       |> Rect.integrate
                       |> (fun paddle -> match !input_state with
                       | { left_pressed = true; right_pressed = false; _ } -> 
                               { paddle with vel = { paddle.vel with x = -1.0 *. paddle_speed } }
                       | { left_pressed = false; right_pressed = true; _ } -> 
                               { paddle with vel = { paddle.vel with x = paddle_speed } }
                       | _ -> { paddle with vel = { x = 0.0; y = 0.0 }})
        ; bottom_paddle = bottom_paddle
                          |> Rect.integrate
                          |> (fun paddle -> match !input_state with
                          | { a_pressed = true; d_pressed = false; _ } -> 
                                  { paddle with vel = { paddle.vel with x = -1.0 *. paddle_speed } }
                          | { a_pressed = false; d_pressed = true; _ } -> 
                                  { paddle with vel = { paddle.vel with x = paddle_speed } }
                          | _ -> { paddle with vel = { x = 0.0; y = 0.0 }})
        ; ball = ball
                 |> Rect.integrate
                 |> (fun ball -> if Rect.left ball <. 0.0 || Rect.right ball >. 800.0 
                                then { ball with vel = { ball.vel with x = ball.vel.x *. -1.0 } }
                                else ball)
                 |> (fun ball -> if Rect.overlap ball top_paddle || Rect.overlap ball bottom_paddle
                                then { ball with vel = { ball.vel with y = ball.vel.y *. -1.0 } <*> 1.1 }
                                else ball)
                 |> (fun ball -> if Rect.top ball <. 0.0 || Rect.bottom ball >. 800.0
                                then { ball with pos = { x = 400.0; y = 400.0 }; vel = random_vel () }
                                else ball)
        ; paddle_speed = if Rect.overlap top_paddle ball || Rect.overlap bottom_paddle ball
                         then paddle_speed *. 1.1 else paddle_speed }
end

let paddle_bounds = Vector.{x = 90.0; y = 10.0}

let game_state = ref GameState.{
    top_paddle = Rect.{ 
        pos = Vector.{x = 400.0 -. paddle_bounds.x /. 2.0; y = 10.0}; 
        bounds = paddle_bounds;
        vel = Vector.{x = 0.0; y = 0.0};
    };
    bottom_paddle = Rect.{
        pos = Vector.{x = 400.0 -. paddle_bounds.x /. 2.0; y = 780.0}; 
        bounds = paddle_bounds;
        vel = Vector.{x = 0.0; y = 0.0};
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
    print_endline "ocaml";
