import "./style.css";

/** All constants used my main() grouped accordingly
 * CONS: All constants
 *  ZERO: Just 0
 *  TICK_SPEED: The speed in ms which the Tick value increases
 *  SVG: Constants related to the svg canvas
 *  FRG: Constants related to the Frog in the game
 *  ENT: Constants related to Entities (Cars, Logs and Turtles)
 *      LAN: Land Entities
 *      RIV: River Entities (Logs and Turtles)
 *  STS: Constants related to statistics of the game
 */
export const CONS = {
    ZERO: 0, FIVE: 5, MAGIC_NUM: 69,
    M: Math.pow(2, 32), C: 134775813, A: 1,
    TICK_SPEED: 10,
    INIT_FAC: 1, FAC_MULT: 0.5,
    SAFE_ROW: 6,

    SVG: {
        WID: 540,
        HGT: 598,
    },
    FRG: {
        DIM: 38, INIT_VEL: 0,
        INIT_X: 270, INIT_Y: 521,
        INIT_RTT: 0, INIT_ROW: 0,
        OS: -10, RTT: 90,
        MOVE_X: 32, MOVE_Y: 38.5, MOVE_ROW: 1
    },
    ENT: {
        ROW_SPACING: 38.5, TRK_ROW: 4,
        TURT_ROW: 3, SINK_COL: 3,
        GTR_ROW: 4, GTR_COL: 3, GTR_HGT: 33,
    },
    LAN: {
        CAR_WID: 33, CAR_HGT: 28.5,
        TRK_WID: 55, TRK_HGT: 24,
        INIT_X: 50, INIT_Y: 487,
        VEL: [-2 / 7, 2 / 7, -0.4, 0.7, -5 / 9],
        AMT: [3, 3, 3, 1, 2],
        OS: [0, 0, 0, 0, 0]
    },
    RIV: {
        // Each row represents a River row
        // Index 0 is the bottom most row and progresses upwards
        X: [[-90, 73, 236, 399],
        [-75, 77, 278, 430],
        [-70, 215, 500],
        [-60, 103, 266, 429],
        [0, 165, 369, 534]],
        Y: 258.5,
        VEL: [-5 / 9, 0.4, 2 / 3, -2 / 3, 0.5],
        WID: [98, 76, 152, 65, 114], HGT: 25,
        AMT: [4, 4, 3, 4, 4],
        OS: [15, 90, 165, 50, 84],
        GTR_WID_FAC: 2 / 3
    },

    FLY: {
        INIT_GOAL_NUM: 355777, X: 43, Y: 68, WIDTH: 23, HEIGHT: 20.5
    },
    STS: {
        INIT_GAME_TIME: -1,
        MAX_LIVES: 2,
        MIN_Y: 59,
        FLY_INV: 250,
        SWIM_INV: 150,
        SINK_INV: 250,
        SINK_TIME: 200,
        ALL_TGT_BNS: 1000,
        BITE_INV: 200,
        SINK_OS: 50,
        FRG_TRANS: 40,
        DTH_INV: 200,
    },
    GOAL: {
        GOAL_SPACING: 108, GOAL_X: 41, GOAL_Y: 62,
        GOAL_SCORE: 50, FLY_SCORE: 200,
        G_MIN_X: 25, G_MAX_X: 46,
    }
} as const,

    svg = document.getElementById("mainCanvas")!