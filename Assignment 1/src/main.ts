import "./style.css";
import { Entity, River, Event, Key, Vec, Frog, Car, State, Fly } from "./types";
import { CONS, svg } from "./consts";
import { startGame } from "./startGame";
import { range, attr, showLives, showScore, showWinFrogs, animateDeath, showFly, showPause } from "./utilityFuncs"

import { fromEvent, interval, merge, Observable, timer } from 'rxjs';
import { map, filter, scan } from 'rxjs/operators';

/**
 * The main function for the frogger game
 */
function main() {
  // Game Clock: Basically time in the game
  class Tick { constructor(public readonly elapsed: number) { } }
  const gameClock$: Observable<Tick> = interval(CONS.TICK_SPEED).pipe(map(elapsed => new Tick(elapsed))),

    /** Function to produce a next random value for Fly goalNum
     *    Every subsequent value is created using the Linear Congruential Generator.
     * @param val The seed value for this rng
     * @returns An object with the current and next (within a function) random value 
     * Source: FIT1008 - S1 2022, Assignment 3 random number generator (for next() property)
    */
    RNG = (val: number) => ({
      value: val,
      next: () => RNG(((val * CONS.A + CONS.C) % CONS.M))
    }),

    /** =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     *  =========================================== OBSERVABLES ===========================================
     *  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
     *  Note: We don't consider keyup since nothing changes when keyup occurs
     *  Source & Credit: Tim's Asteroids code (altered slightly)
     */

    /** Function to create a Key Observable
     * @param e The event type of the key observable
     * @param kArr An array of keys which are valid for the observable
     * @param output A function determining what the Observable emits
     * @returns A key observable based on the input given
     */
    keys$ = (e: Event) => (kArr: Key[]) => <T>(output: () => T): Observable<T> =>
      fromEvent<KeyboardEvent>(document, e)
        .pipe(
          filter(({ key }) => kArr.reduce((bool, k) => bool || key.toLowerCase() === k, false)),
          filter(({ repeat }) => !repeat), map(output)
        ),

    // Initialising Key Observables
    pause$ = keys$("keydown")(["p"])(() => <Vec>{ x: CONS.ZERO, y: CONS.ZERO, rotate: CONS.MAGIC_NUM }),

    moveFwd$: Observable<Vec> = keys$("keydown")(["w", "arrowup"])(() => <Vec>{
      x: CONS.ZERO, y: -CONS.FRG.MOVE_Y,
      row: CONS.FRG.MOVE_ROW, rotate: CONS.ZERO,
    }),
    moveBackward$: Observable<Vec> = keys$("keydown")(["s", "arrowdown"])(() => <Vec>{
      x: CONS.ZERO, y: CONS.FRG.MOVE_Y,
      row: -CONS.FRG.MOVE_ROW, rotate: 2 * CONS.FRG.RTT,
    }),
    moveLeft$: Observable<Vec> = keys$("keydown")(["a", "arrowleft"])(() => <Vec>{
      x: -CONS.FRG.MOVE_X, y: CONS.ZERO,
      row: CONS.ZERO, rotate: 3 * CONS.FRG.RTT,
    }),
    moveRight$: Observable<Vec> = keys$("keydown")(["d", "arrowright"])(() => <Vec>{
      x: CONS.FRG.MOVE_X, y: CONS.ZERO,
      row: CONS.ZERO, rotate: CONS.FRG.RTT,
    }),
    reset$: Observable<Vec> = keys$("keydown")(["r"])(() => <Vec>{ x: CONS.ZERO, y: CONS.ZERO }),

    // pause$ = keys$("keydown")(["p"])(() => <Vec>{ x: CONS.ZERO, y: CONS.ZERO, rotate: CONS.MAGIC_NUM }),

    /** =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     *  ===================================== CREATING INITIAL STATE ======================================
     *  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */

    /** 
     * Function to create our frog object in the game state
     *    Used in initialState
     * @returns A frog with all it's starting values
     */
    createFrog = (): Frog => ({
      id: "froggy", row: CONS.ZERO,
      x: CONS.FRG.INIT_X, y: CONS.FRG.INIT_Y, vel: CONS.ZERO,
      width: CONS.FRG.DIM, height: CONS.FRG.DIM,
      rotate: CONS.ZERO, moving: false,
      collided: false, deathDetails: {
        deathBy: null, x: CONS.ZERO, y: CONS.ZERO
      },
    }),

    /** 
     * Function to create all 5 rows of cars
     *    Used in initialState
     * @returns All 5 rows of vehicles with their respective starting values
     */
    startCars = (): Car[][] => range(CONS.FIVE)
      .map((row: number): Car[] => range(CONS.LAN.AMT[row])
        .map((col: number): Car =>
        ({
          id: "veh" + row + col, vel: CONS.LAN.VEL[row],
          y: CONS.LAN.INIT_Y - row * CONS.ENT.ROW_SPACING, offset: CONS.LAN.OS[row],
          ...row === CONS.ENT.TRK_ROW ?
            { // The starting x value is "randomised" as such
              x: CONS.LAN.INIT_X + CONS.ENT.TRK_ROW * col * CONS.LAN.TRK_WID,
              width: CONS.LAN.TRK_WID, height: CONS.LAN.TRK_HGT
            }
            : {
              x: row * CONS.LAN.INIT_X + CONS.FIVE * col * CONS.LAN.CAR_WID,
              width: CONS.LAN.CAR_WID, height: CONS.LAN.CAR_HGT
            }
        }))),

    /**
     * Function to create all 5 river rows
     *    Used in initialState
     * @returns All 5 river rows with their respective starting values
     */
    startRivers = (): River[][] => range(CONS.FIVE)
      .map((row: number): River[] => range(CONS.RIV.AMT[row])
        .map((col: number): River => {
          const isGator = row === CONS.ENT.GTR_ROW && col === CONS.ENT.GTR_COL
          return ({
            id: "riv" + row + col, offset: CONS.RIV.OS[row],
            x: CONS.RIV.X[row][col], y: CONS.RIV.Y - row * CONS.ENT.ROW_SPACING,
            vel: CONS.RIV.VEL[row], width: CONS.RIV.WID[row],
            height: isGator ? CONS.ENT.GTR_HGT : CONS.RIV.HGT,
            sinking: (!row || (row === CONS.ENT.TURT_ROW)) && (col === CONS.ENT.SINK_COL),
            sunk: false, isGator: isGator
          })
        })),

    /** Function to create a new set of unfilled targets */
    startTargets = (): boolean[] => [false, false, false, false, false],

    /** Function to create a new Fly object */
    startFly = (): Fly => ({
      appear: false, goalNum: CONS.FLY.INIT_GOAL_NUM, rng: RNG(CONS.FLY.INIT_GOAL_NUM),
      x: CONS.FLY.X, y: CONS.FLY.Y,
      width: CONS.FLY.WIDTH, height: CONS.FLY.HEIGHT
    }),

    /**
     * Function to create the initial state of the game
     * @returns The initial game state
     */
    initialState = (): State => ({
      gametime: CONS.STS.INIT_GAME_TIME, level: CONS.ZERO,
      frog: createFrog(),
      carRows: startCars(), riverRows: startRivers(),
      fly: startFly(),
      targets: startTargets(),
      highScore: CONS.ZERO, highestRow: CONS.ZERO,
      score: CONS.ZERO, lives: CONS.STS.MAX_LIVES,
      gameOver: false, paused: false, reset: false,
    }),

    /** =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     *  ===================================== UPDATING GAME STATE =========================================
     *  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */

    /** 
     * Checking if the x value of the frog is within a goal 
     * @param s The current game state
     * @returns The updated game state after checking accordingly
     */
    checkGoal = (s: State): State => {
      const targetNum = Math.floor(s.frog.x / CONS.GOAL.GOAL_SPACING),  // The nearest target 
        modX = s.frog.x % CONS.GOAL.GOAL_SPACING,                       // The relative x value for each goal segment
        newTargets = s.targets                                          // Storing a reference of the targets list

      // Check if the x value is within a target spacing
      if (CONS.GOAL.G_MIN_X <= modX && modX <= CONS.GOAL.G_MAX_X && !s.targets[targetNum]) {
        // Indicate the target has been filled and update the gamestate accordingly
        newTargets[targetNum] = true
        const scoreInc = 50 + (s.fly.goalNum === targetNum && s.fly.appear ? 200 : 0)
        return { ...s, score: s.score + scoreInc, frog: createFrog(), highestRow: 0 }
      }
      // else, end the current game attempt
      return {
        ...s, ...s.lives ?
          {
            frog: {
              ...createFrog(), collided: true,
              deathDetails: { deathBy: "collision", x: s.frog.x, y: s.frog.y }
            }, lives: s.lives - 1
          }
          : { gameOver: true }
      }
    },

    /** 
     * Updating the frog state when it moves
     * @param frog The current frog in the game state
     * @param e    The Vec value emitted by a Key Observable
     * @returns The updated frog
     */
    reduceFrog = (f: Frog, e: Vec): Frog => {
      // new x value and if x changes or not
      const newX = f.x + e.x,
        xMoved = newX > 0 && newX < 500,

        // new y value and if y changes or not (and hence if row changes)
        newY = f.y + e.y,
        yMoved = newY <= CONS.FRG.INIT_Y && newY >= CONS.STS.MIN_Y,
        newRow = yMoved ? f.row + e.row : f.row;

      return {
        ...f,
        row: newRow,
        x: xMoved ? newX : f.x,
        y: yMoved ? newY : f.y,
        vel: newRow > 6 && newRow < 12 ?
          CONS.RIV.VEL[newRow - 7] : 0, // The frog only has vel when it is on a river row
        rotate: e.rotate,
        moving: true,
      }
    },

    /** 
     * Updating game state either via game tick or user input
     * @param s The current game state
     * @param event The cause of the change of game state (either via Tick or Vec)
     * @returns The updated game state
     */
    reduceState = (s: State, event: Vec | Tick): State => {
      const factor = CONS.INIT_FAC + CONS.FAC_MULT * s.level,
        newX = s.frog.x + s.frog.vel * factor,  // froggy x value after adding vel

        /**
         * Function to update the Frog state values based on both user input and its vel
         * @returns The game state after moving the Frog state
         */
        moveFrog = (): State => {
          const newFrog = reduceFrog(s.frog, event as Vec)
          return ({
            ...s,
            ...newFrog.row > s.highestRow ? { score: s.score + 10, highestRow: s.highestRow + 1 } : {},
            frog: newFrog
          })
        },

        /** This function wraps the background such that when an entity goes off-screen, 
         *    it will return to the start of its track and be re-used
         * @param eDim The width of the input entity
         * @param size The size of the background = 540
         * @param e    The Entity which we are wrapping around the background
         * @returns    The x-coordinate of the entity after wrapping
         * Source: Tim's Asteroids Code (altered slightly)
         */
        wrap = (eDim: number, size: number = CONS.SVG.WID) => (e: Entity): number => {
          const point = e.x + e.vel * factor,
            os = e.offset

          return point < -eDim ?      // If e is completely hidden on the left of the background
            size + os :
            point > size + os ?       // If e is completely hidden on the right of the background
              -eDim :
              point                   // else
        },

        /** 
         * To move an entire row using moveEntity
         * @param rows A row to be moved
         * @returns The row after moving
         */
        moveRows = (rows: Entity[][]): Entity[][] =>
          rows.map((row: Entity[]): Entity[] =>
            row.map((item: Entity): Entity => {
              const newItem = { ...item, x: wrap(item.width)(item) }
              return Object.hasOwn(item, "sinking") ?
                {
                  ...newItem, sunk: (item as River).sinking &&
                    s.gametime % CONS.STS.SINK_INV > CONS.STS.SINK_TIME
                }
                : newItem
            })),

        /** 
         * Function to update fly object 
         * @param f The fly object in the current game state
         * @returns The updated fly object
        */
        reduceFly = (f: Fly): Fly => {
          const rand = f.rng.next(),
            gNum = Math.floor(rand.value % CONS.FIVE)
          return s.gametime % CONS.STS.FLY_INV === CONS.ZERO ?
            {
              ...f, appear: !f.appear, goalNum: gNum, rng: rand,
              x: CONS.FLY.X + gNum * CONS.GOAL.GOAL_SPACING, y: CONS.FLY.Y
            }
            : f
        },

        /** 
         * Checks if collision has occured
         * @param s The current game state
         * @returns The updated game state
         */
        collisionCheck = (s: State): State => {
          const rowIndex = s.frog.row % CONS.SAFE_ROW - 1, // Row index value used at the end of this function
            /** 
             * Checks if a frog has hit a vehicle or touched the river
             * @param row A row of entities
             * @returns A boolean value stating if a frog has hit a vehicle or touched the river
             * Source: FIT2102 - Workshop 2, Overlapping Rectangles (altered slightly)
             */
            overlap = (row: Entity[], isRiver: boolean = false) => {
              const lowX = s.frog.x,
                highX = lowX + s.frog.width,

                compare = (bool: boolean, e: Entity): boolean =>
                  // For River, need to consider sinking Turtles and Gators
                  isRiver ?
                    (e as River).sunk ? bool
                      : bool || !(e.x > highX + CONS.FRG.OS
                        || e.x + ((e as River).isGator ? CONS.RIV.GTR_WID_FAC : CONS.INIT_FAC)
                        * e.width < highX + CONS.FRG.OS)

                    // Basic comparison for Car Entities
                    : bool || !(e.x > highX + CONS.FRG.OS || e.x + e.width < lowX - CONS.FRG.OS),

                overlapping = row.reduce((bool: boolean, e: Entity): boolean => compare(bool, e), false)
              return isRiver ? !overlapping : overlapping
            },

            /**
             * Function to execute when froggy is on the road or river
             * It checks both if the frog has collided as well as if the frog has anymore lives
             * @returns The updated game state
             */
            onRoadOrRiver = (): State => {
              const onRoad = s.frog.row in range(CONS.SAFE_ROW),
                collided = onRoad ? overlap(s.carRows[rowIndex]) : overlap(s.riverRows[rowIndex], true),
                offScreen = newX < CONS.FRG.OS || newX > CONS.SVG.WID - CONS.FRG.DIM,
                deathBy = offScreen || onRoad ? "collision" : "drown"

              return offScreen || collided ? {
                ...s, frog: {
                  ...createFrog(), collided: true,
                  deathDetails: { deathBy: deathBy, x: s.frog.x, y: s.frog.y }
                }, lives: s.lives - 1, gameOver: !s.lives
              }
                : s
            }

          return !s.frog.row || s.frog.row === CONS.SAFE_ROW ? s   // If froggy is on a safe zone
            : s.frog.row === 12 ? checkGoal(s)              // If froggy is at the final row
              : onRoadOrRiver()                             // If froggy is on the road or river
        }

      return event instanceof Tick ?
        // Collision occured, current game has ended
        s.gameOver ? s.frog.collided ? { ...s, frog: { ...s.frog, collided: false } } : s

          // Every target has been filled
          : s.targets.every(_ => _) ? {
            ...s, frog: createFrog(),
            gametime: s.gametime + 1,
            targets: startTargets(),
            level: s.level + 1,
            score: s.score + CONS.STS.ALL_TGT_BNS
          }
            // Game is paused, no need to update anything
            : s.paused ? s
              // Normal scenario, a Tick has passed
              : collisionCheck({
                ...s,
                gametime: s.gametime + 1,
                frog: {
                  ...s.frog,
                  x: newX,
                  moving: false,
                  collided: false,
                  deathDetails: { deathBy: null, x: CONS.ZERO, y: CONS.ZERO }
                },
                carRows: <Car[][]>moveRows(s.carRows),
                riverRows: <River[][]>moveRows(s.riverRows),
                fly: reduceFly(s.fly),
                highScore: Math.max(s.highScore, s.score),
                reset: false
              })

        // User input detected, frog moved
        : event.x || event.y ? moveFrog()
          // User pressed R to restart
          : s.gameOver ? { ...initialState(), gametime: s.gametime, highScore: s.highScore, reset: true }
            : event.rotate === CONS.MAGIC_NUM ? { ...s, paused: !s.paused }
              : s
    }

  /** =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   *  =========================================== UPDATE VIEW ===========================================
   *  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
  /**
   * Function to update every View item (frogs, vehicles, river stuff, croc, snake, etc)
   * @param s The current game state (back-end)
   * @postCondition Every View item (front-end) is updated based on the current game state, s (back-end)
   */
  function updateView(s: State) {
    // If the current attempt is over
    if (s.gameOver) {
      attr(document.getElementById("froggy")!)([["visibility", "hidden"]]);
      if (s.frog.collided) {
        ([document.getElementById("gameOver")!, document.getElementById("gameOverLine1")!, document.getElementById("gameOverLine2")!])
          .forEach(img => { svg.insertBefore(img, null); attr(img)([["visibility", "visible"]]) })
        showLives(s.lives);
        animateDeath(s)
      } return
    }

    // If the game has just started
    !s.gametime ? startGame()
      // If the collision occured
      : s.frog.collided ? animateDeath(s) : {};

    ([document.getElementById("gameOver")!, document.getElementById("gameOverLine1")!, document.getElementById("gameOverLine2")!])
      .forEach(img => { attr(img)([["visibility", "hidden"]]) })

    /** =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     *  ======================================== UPDATING FROG VIEW =======================================
     *  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */
    const frogView: Element = document.getElementById("froggy")!, frogState: Frog = s.frog,
      frogAttr = Object.entries({
        ...frogState, visibility: "visible",
        transform: `rotate(${frogState.rotate}, ${String(frogState.x + 19)}, ${String(frogState.y + 19)})`,
      }),
      setFrog = attr(frogView)
    svg.insertBefore(frogView, null)
    setFrog(frogAttr)

    // If input is given, we move the frog accordingly
    if (s.frog.moving) {
      setFrog([["visibility", "hidden"]])    // Hide original frog to show transition frog first

      // Creating transition frog for animation
      const midFrogView: Element = document.getElementById("midFroggy")!,
        setMidFrog = attr(midFrogView)
      setMidFrog([...frogAttr, ["id", midFrogView.getAttribute("id")], ["visibility", "visible"]])

      // Transition frog appears for 30ms before original takes over again
      timer(CONS.STS.FRG_TRANS).subscribe(() => { setMidFrog([["visibility", "hidden"]]); setFrog([["visibility", "visible"]]) })
    }

    /** =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     *  ===================================== UPDATING ENTITY VIEW ========================================
     *  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= */

    /** 
     * Function to update the Entity image elements
     * @param row       The current row the Entity is on
     * @param eState    The current Entity being updated
     * @param isRiver  A boolean stating if the Entity is a River or not
     */
    const updateElement = (row: number) => (eState: Entity, isRiver: boolean = false): void => {
      /**
       * Function to animate the Turtle swimming (or sinking)
       *    This is done by checking the value of the current game time to determine which png the turtle should have
       * @param t A River entity
       */
      const swim = (t: River): void => {
        const tickPhase = t.sinking ? s.gametime % CONS.STS.SINK_INV + CONS.STS.SINK_OS : s.gametime % CONS.STS.SWIM_INV,
          turleBoi = document.getElementById(t.id)!,
          png =
            tickPhase <= 50 ? "../assets/river_" + row + ".png"
              : tickPhase <= 100 ? "../assets/river_" + row + "1.png" : tickPhase <= 150 ? "../assets/river_" + row + "2.png"
                : tickPhase <= 200 ? "../assets/river_sink_" + row + "1.png" : tickPhase <= 250 ? "../assets/river_sink_" + row + "2.png"
                  : ""
        attr(turleBoi)([["href", png]])
      },

        /**
         * Function to animate the Gator biting
         *    This is done by checking the value of the current game time to determine which png the Gator should have
         * @param g The Gator entity to animate
         */
        bite = (g: River): void => {
          attr(document.getElementById(g.id)!)([["href", "../assets/gator_" + Number(s.gametime % CONS.STS.BITE_INV >= 100) + ".png"]])
        }

      attr(document.getElementById(eState.id)!)(Object.entries(eState))
      isRiver ? (!row || row === 3) ? swim(<River>eState) : ((eState as River).isGator) ? bite(<River>eState) : undefined : undefined
    }

    // Updating the Entity Views
    range(CONS.FIVE).forEach(row => {
      const updating = updateElement(row)
      s.carRows[row].forEach(car => updating(car))
      s.riverRows[row].forEach(riv => updating(riv, true))
    })

    // Displaying the minors details on the screen
    showWinFrogs(s.targets)
    showFly(s.fly, s.targets)
    showScore(s.score)
    showScore(s.highScore, true)
    showLives(s.lives)
    showPause(s.paused);
  }


  // Game logic here
  (merge(
    pause$, gameClock$,
    moveFwd$, moveBackward$,
    moveLeft$, moveRight$,
    reset$) as Observable<Tick | Vec>)
    .pipe(scan(reduceState, initialState()))     // Updating the back end values/elements
    .subscribe(updateView);                   // Updating the front end values/elements
}

// The following simply runs your main function on window load.  Make sure to leave it in place.
if (typeof window !== "undefined") {
  window.onload = () => { main(); };
}