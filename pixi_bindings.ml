open Js_of_ocaml

class type application = object
    method view : Dom.node Js.t
end

let application = Js.Unsafe.global##._PIXI##._Application

class type graphics = object
    method beginFill : int -> unit Js.meth
    method drawRect : (int * int * int * int) -> unit Js.meth
    method clear : unit -> unit Js.meth
end

let graphics = Js.Unsafe.global##.PIXI##._Graphics

let pixi_setup () =
    let graphics = new%js graphics () in
    let _ = graphics##beginFill 0xffffff in

    let app = new%js application object%js val width = 800.0; val height = 800.0 end in
    let _ = Dom.appendChild Dom_html.document##.body app##.view
    and _ = app##.stage##addChild(graphics) in

    let _ = Js.Unsafe.global##addEventListener "keyup" Input.handle_keyup
    and _ = Js.Unsafe.global##addEventListener "keydown" Input.handle_keydown
    in (graphics, app)
