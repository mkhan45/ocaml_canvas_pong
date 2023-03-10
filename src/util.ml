open Core

let (>.) = Float.(>)
let (<.) = Float.(<)

module Vector = struct
    type t = {x: float; y: float} [@@deriving lens]

    let zero = { x = 0.0; y = 0.0 }

    let magnitude { x; y } = Float.sqrt ((x *. x) +. (y *. y))

    module Ops = struct
        let (<+>) a b = { x = a.x +. b.x; y = a.y +. b.y }
        let (<*>) a s = { x = a.x *. s; y = a.y *. s }
    end
end

open Vector.Ops
module Rect = struct
    type t = {pos: Vector.t; bounds: Vector.t; vel: Vector.t} [@@deriving lens]

    let top rect = rect.pos.y
    let bottom rect = rect.pos.y +. rect.bounds.y

    let left rect = rect.pos.x
    let right rect = rect.pos.x +. rect.bounds.x

    let overlaps a b = List.for_all ~f:(phys_equal false) [
        left a >. right b;
        right a <. left b;
        top a >. bottom b;
        bottom a <. top b;
    ]

    let integrate rect = { rect with pos = rect.pos <+> rect.vel }
end
