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
