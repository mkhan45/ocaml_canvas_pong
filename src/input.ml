open Js_of_ocaml

(* Stores pressed inputs, since JS only gives events *)
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
