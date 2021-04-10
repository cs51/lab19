(*
                             CS 51 Lab 19
                Greenberg-Hastings Cellular Automaton
 *)
(*
    **************************************************************
    WARNING: If you have photosensitive epilepsy, you should avoid
    viewing the Greenberg-Hastings cellular automaton. It exhibits
    rapidly flashing visual patterns.
    **************************************************************
 *)

module G = Graphics ;;
  
(* Automaton parameters *)
let cGRID_SIZE = 100 ;;       (* width and height of grid in cells *)
let cSPARSITY = 25 ;;         (* inverse of proportion of cells initially live *)

(* Rendering parameters *)
let cCOLOR_LEGEND = G.rgb 173 106 108 ;;  (* color for textual legend *)
let cSIDE = 8 ;;              (* width and height of cells in pixels *)
let cRENDER_FREQUENCY = 1     (* how frequently grid is rendered (in ticks) *) ;;
let cFONT = None ;;

type gh_state =
  | Resting
  | Excited
  | Refractory
      
(* offset index off -- Returns the `index` offset by `off` allowing
   for wraparound. *)
let rec offset (index : int) (off : int) : int =
  if index + off < 0 then
    cGRID_SIZE - (offset ~-index ~-off)
  else
    (index + off) mod cGRID_SIZE ;;

(* gh_update grid i j -- *)
let gh_update (grid : gh_state array array) (i : int) (j : int) =
  let neighbors = ref false in
  for i' = ~-1 to ~+1 do
    for j' = ~-1 to ~+1 do
      let i_ind = offset i i' in
      let j_ind = offset j j' in
      if grid.(i_ind).(j_ind) = Excited then
        neighbors := true
    done
  done;
  match grid.(i).(j) with
  | Excited -> Refractory
  | Refractory -> Resting
  | Resting -> if !neighbors then Excited else Resting ;;
  
module GHSpec : (Cellular.AUT_SPEC
                 with type state = gh_state) =
  struct 
    type state = gh_state
    let name : string = "Greenberg-Hastings"
    let initial : state = Resting
    let grid_size : int = cGRID_SIZE
    let update = gh_update
    let cell_color (cell : state) : G.color =
      match cell with
      | Resting -> G.rgb 245 240 246
      | Excited -> G.rgb 56 95 113
      | Refractory -> G.rgb 43 65 98
    let side_size : int = cSIDE
    let legend_color : G.color = cCOLOR_LEGEND
    let font : string option = cFONT
    let render_frequency : int = cRENDER_FREQUENCY
  end ;;
  
module Aut = Cellular.Automaton (GHSpec) ;;

(* Some initial grids *)

(* random_grid count -- Returns a grid with cells set to live at
   `count` random locations. *)
let random_grid count =
  let mat = Aut.fresh_grid () in
  for _ = 1 to count do
    mat.(Random.int cGRID_SIZE).(Random.int cGRID_SIZE) <-
      if Random.bool () then Excited else Refractory
  done;
  mat ;;                       

(*......................................................................
  Running the game and displaying the results
 *)

(* main seed -- Runs the game using the provided random `seed` (for
   replicability), displaying updates to the graphics window. *)
let main seed =
  
  Random.init seed;      (* initialize randomness *)
  Aut.graphics_init ();  (* ... and graphics window *)

  let initial_grid = random_grid (cGRID_SIZE * cGRID_SIZE / cSPARSITY) in
  Aut.run_grid initial_grid ;;

(* Run the game with user-supplied seed *)
let _ =
  if Array.length Sys.argv <= 1 then
    Printf.printf "Usage: %s <seed>\n  where <seed> is integer seed\n"
                  Sys.argv.(0)
  else
    main (int_of_string Sys.argv.(1)) ;;
  
