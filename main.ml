open Core

open Pixi_bindings
open Js_of_ocaml

let (>.) = Float.(>)
let (<.) = Float.(<)

let graphics = 
    let graphics = new%js graphics () in
    let _ = graphics##beginFill 0xffffff
    in graphics

let app = 
    let app = new%js application object%js val width = 800.0; val height = 800.0 end in
    let _ = Dom.appendChild Dom_html.document##.body app##.view
    and _ = app##.stage##addChild(graphics) in
    app

module Vector = struct
    type t = {x: float; y: float}

    module Ops = struct
        let (<+>) a b = { x = a.x +. b.x; y = a.y +. b.y }
    end
end
open Vector.Ops

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

module GameState = struct
    type t = { top_paddle: Rect.t; bottom_paddle: Rect.t; ball: Rect.t }

    let draw game =
        let _ = graphics##clear in
        let _ = graphics##beginFill 0xffffff in
        let _ = Rect.draw game.top_paddle
        and _ = Rect.draw game.bottom_paddle
        and _ = Rect.draw game.ball in
        ()

    let updated { top_paddle; bottom_paddle; ball } =
        { top_paddle = Rect.integrate top_paddle
        ; bottom_paddle = Rect.integrate bottom_paddle
        ; ball = ball
                 |> Rect.integrate
                 |> fun ball -> if Rect.left ball <. 0.0 || Rect.right ball >. 800.0 
                                then { ball with vel = { x = -1.0 *. ball.vel.x; y = ball.vel.y } }
                                else ball
                 |> fun ball -> if Rect.top ball <. 0.0 || Rect.bottom ball >. 800.0 
                                then { ball with vel = { y = -1.0 *. ball.vel.y; x = ball.vel.x } }
                                else ball
        }
end

let paddle_bounds = Vector.{x = 70.0; y = 10.0}

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
        vel = Vector.{x = 1.0; y = 2.0};
    };
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
