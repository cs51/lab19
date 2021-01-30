(*
                                 CS51
                   2-dimensional cellular automata
 *)

module G = Graphics ;;

(*......................................................................
  Specifying automata 

  A cellular automaton is a grid of cells each in a specific
  state. The grid is destructively updated according to an update
  rule. The evolving state of the CA can be visualized by displaying
  the grid in a graphics window. 

  In this implementation, an automation is specified by a module
  satisfying the following signature: *)
   
module type AUT_SPEC =
  sig
    (* Automaton parameters *)
    type state                 (* the space of possible cell states *)
    val initial : state        (* the initial (default) cell state *)
    val grid_size : int        (* width and height of grid in cells *)
    val update : (state array array) -> int -> int -> state
                               (* update grid i j -- returns the new
                                  state for the cell at position `i, j`
                                  in `grid` *)
    (* Rendering parameters *)
    val name: string           (* a display name for the automaton *)
    val side_size : int        (* width and height of cells in pixels *)
    val cell_color : state -> G.color
                               (* color for each state *)
    val legend_color : G.color (* color to render legend *)
    val render_frequency : int (* how frequently grid is rendered (in ticks) *)
  end ;;

(*......................................................................
  Implementing automata based on a specification

  This implementation provides for the following functionality:

  * Creating a grid, which can then be updated by the receiver to form
    the initial grid of a trial.

  * Initializing the graphics window in preparation for rendering.

  * Mapping an update function simultaneously over all cells of a grid.

  * Running the automaton using a particular update function, and
    rendering it as it evolves. 

An automaton thus satisfies the following signature: *)
  
module type AUTOMATON =
  sig
    (* state -- Possible states that a grid cell can be in *)
    type state
    (* grid -- 2D grid of cell states *)
    type grid = state array array
    (* create_grid () -- Returns a grid with all cells in initial
       states. *)
    val create_grid : unit -> grid
    (* graphics_init () -- Initialize the graphics window to the
       correct size and other parameters. Auto-synchronizing is off,
       so our code is responsible for flushing to the screen upon
       rendering. *)
    val graphics_init : unit -> unit
    (* step_grid grid -- Updates the `grid` by updating each cell
       simultaneously with as per the CA's update rule. *)
    val step_grid : grid -> unit
    (* run_grid grid update -- Starts up the automaton on the provided
       initial `grid` using the `update` rule, rendering to the
       graphics window until a key is pressed. The `update` rule is a
       function operating as per `map_grid`. Assumes graphics has been
       initialized. *)
    val run_grid : grid -> unit
  end ;;

module Automaton (Spec : AUT_SPEC)
       : (AUTOMATON with type state = Spec.state) =
  struct
  (* COMPLETE THE IMPLEMENTATION OF THIS FUNCTOR. You'll undoubtedly
     find the code in life.ml useful. Much of the needed code can be
     cut-and-pasted from there, with appropriate modifications. *)
  end ;;
