(*
                             CS 51 Lab 19
                   Brian's Brain Cellular Automaton
 *)

module G = Graphics ;;
  
(* Automaton parameters *)
let cGRID_SIZE = 100 ;;       (* width and height of grid in cells *)
let cSPARSITY = 5 ;;          (* inverse of proportion of cells initially live *)

(* Rendering parameters *)
let cCOLOR_LIVE = G.rgb 200 200 200 ;;       (* color to depict live cells *)
let cCOLOR_DYING = G.rgb 130 0 0 ;;   (* color to depict dying cells *)
let cCOLOR_DEAD = G.rgb 0 0 0 ;;          (* background color *)
let cCOLOR_LEGEND = G.rgb 173 106 108 ;;  (* color for textual legend *)
let cSIDE = 8 ;;              (* width and height of cells in pixels *)
let cRENDER_FREQUENCY = 1     (* how frequently grid is rendered (in ticks) *) ;;
let cFONT = None ;;
              
(*----------------------------------------------------------------------
  Brian's Brain Cellular Automaton 
 *)

(*......................................................................
  States, updates, and utility functions
 *)

(* We take the opportunity to abstract the cell states as an
   enumerated type *)
type bbrain_state =
  | Live
  | Dying
  | Dead ;;
 
(* flipped state -- Returns the opposite state to `state`. *)
let flipped (state : bbrain_state) : bbrain_state =
  match state with
  | Live -> Live
  | Dying -> Dying
  | Dead -> Dead ;;
      
(* offset index off -- Returns the `index` offset by `off` allowing
   for wraparound. *)
let rec offset (index : int) (off : int) : int =
  if index + off < 0 then
    cGRID_SIZE - (offset ~-index ~-off)
  else
    (index + off) mod cGRID_SIZE ;;

(* bbrain_update grid i j -- Returns the updated value for cell at `i,
   j` in the grid based on rules of Brian's Brain
   <https://en.wikipedia.org/wiki/Brian%27s_Brain>. *)
let bbrain_update (grid : bbrain_state array array)
                  (i : int) (j : int)
    : bbrain_state =
  let neighbors = ref 0 in
  for i' = ~-1 to ~+1 do
    for j' = ~-1 to ~+1 do
      let i_ind = offset i i' in
      let j_ind = offset j j' in
      neighbors := !neighbors
                   + match grid.(i_ind).(j_ind) with
                     | Live -> 1
                     | Dying -> 0
                     | Dead -> 0
    done
  done;
  (* determine new state based on neighbor count *)
  let new_state =
    match grid.(i).(j) with
    | Live -> Dying
    | Dying -> Dead
    | Dead -> if !neighbors = 2 then Live else Dead in
  new_state ;;
  
(*......................................................................
  Making Brian's Brain
 *)

(* Automaton specification *)
  
module BBrainSpec : (Cellular.AUT_SPEC
                     with type state = bbrain_state) =
  struct 
    type state = bbrain_state
    let initial : state = Dead
    let grid_size : int = cGRID_SIZE
    let update = bbrain_update
    let name : string = "Brain's Brain"
    let cell_color (cell : state) : G.color =
      match cell with
      | Live -> cCOLOR_LIVE
      | Dying -> cCOLOR_DYING
      | Dead -> cCOLOR_DEAD
    let side_size : int = cSIDE
    let legend_color : G.color = cCOLOR_LEGEND
    let font : string = cFONT
    let render_frequency : int = cRENDER_FREQUENCY
  end ;;

(* GoL automaton *)
  
module Aut = Cellular.Automaton (BBrainSpec) ;;

(* Initial grids *)

(* random_grid count -- Returns a grid with cells set to live at
   `count` random locations. *)
let random_grid count =
  let the_grid = Aut.fresh_grid() in
  for _ = 1 to count do
    the_grid.(Random.int cGRID_SIZE).(Random.int cGRID_SIZE) <- Live
  done;
  the_grid ;;                       

(*......................................................................
  Running the game and displaying the results
 *)

(* main seed -- Runs the game using the provided random `seed` (for
   replicability), displaying updates to the graphics window. *)
let main seed =
  
  Random.init seed;      (* initialize randomness *)
  Aut.graphics_init ();  (* ... and graphics window *)
  (* Run the automaton starting with a random grid *)
  let initial_grid =
    random_grid (cGRID_SIZE * cGRID_SIZE / cSPARSITY) in
  Aut.run_grid initial_grid ;;

(* Run the game with user-supplied seed *)
let _ =
  if Array.length Sys.argv <= 1 then
    Printf.printf "Usage: %s <seed>\n  where <seed> is integer seed\n"
                  Sys.argv.(0)
  else
    main (int_of_string Sys.argv.(1)) ;;
  

