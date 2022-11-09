import { svg, CONS } from "./consts";
import { State, Fly } from "./types";

import { timer } from "rxjs";

/** =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 *  ======================================== UTILITY FUNCTIONS ========================================
 *  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
 *  Note: Utility functions are also used in startGame (hence the export keyword is used here as well)
 */

export
    /**
     * Function to produce a list of numbers from 0 to n-1
     * @param n   The upper bound (exclusive) of the output list
     * @returns   A list of numbers from 0 to n-1
     * Source: FIT2102 - Week 2, Tutorial 2, Exercise 5
     */
    const range = (n: number): number[] => n ? range(n - 1).concat(n - 1) : [],

    /**
     * Function to set the attributes of v using the input key_val
     * @param v       A html element
     * @param key_val A list of key, value pairs which are attribute keys and values to be set to v
     * Source: Tim's Asteroids Code (changed to become curried)
     */
    attr = (v: Element) => <T>(key_val: [string, T][]): void =>
        key_val.forEach(([key, val]) => v.setAttribute(key, String(val))),

    /**
     * Function to create an image element
     * @returns       A fresh, brand new image element with no attributes set
     */
    createImg = (): Element => document.createElementNS(svg.namespaceURI, "image"),

    /**
     * Function to convert a number to a list of digits (e.g. 96024 -> [9, 6, 0, 2, 4])
     * @param num     A number to be converted
     * @returns       A list of digits of num
     */
    numToList = (num: number): number[] => Array.from(String(num)).map(Number),

    /**
     * Function to display the targets that have been filled
     *  Filled targets are indicated by the "Win Frog" icon
     * @param targets A list of boolean values stating if the corresponding goal has been filled
     */
    showWinFrogs = (targets: boolean[]): void => {
        range(CONS.FIVE).forEach((ind) => {
            const wFrog = document.getElementById("winFrog" + ind)!
            attr(wFrog)([["visibility", targets[ind] ? "visible" : "hidden"]])
        })
    },

    /**
     * Function to show the fly image
     * @param f The fly from the current game state
     * @param targets An array of boolean values representing the targets in the game
     */
    showFly = (f: Fly, targets: boolean[]): void => {
        attr(document.getElementById("fly")!)(f.appear && !targets[f.goalNum] ? Object.entries({ ...f, visibility: "visible" }) : [["visibility", "hidden"]])
    },

    /** 
     * Function to display some score on the screen
     * @param score The current score
     * @param isHigh A boolean stating if the score input is the high score or the current score
     */
    showScore = (score: number, isHigh: boolean = false): void => {
        const scoreDgts: number = score ? Math.floor(Math.log10(score)) + 1 : 1,
            scoreList: number[] = numToList(score),
            svgId: string = isHigh ? "hsSvg" : "scoreSvg",
            numId: string = isHigh ? "hsNum" : "scoreNum"

        range(CONS.FIVE).forEach((dgtNum: number) => {
            const
                scoreSvg: Element = document.getElementById(svgId + dgtNum)!,
                scoreNum: Element = document.getElementById(numId + dgtNum)!,
                scoreDgt: number = scoreList[dgtNum],
                viewBoxX: number = dgtNum < scoreDgts ? 2 * scoreDgt : -2,
                scoreAttr: [string, string][] = [["viewBox", String(viewBoxX) + " -4.5 90 100"]]

            attr(scoreSvg)(scoreAttr), attr(scoreNum)([["width", String(viewBoxX + 2)]])
        })
    },

    /**
     * Function to display the number of lives
     * @param lives The number of lives left
     */
    showLives = (lives: number): void => {
        range(3).forEach(num => {
            const lifeFrog = document.getElementById("lifeFrog" + num)!
            attr(lifeFrog)([["visibility", num <= lives ? "visible" : "hidden"]])
        })
    },

    /**
     * Function to show the "PAUSED" image
     * @param show A boolena value stating if the game has been paused or not
     */
    showPause = (show: boolean) => { attr(document.getElementById("paused")!)([["visibility", show ? "visible" : "hidden"]]) },

    /**
     * Function to play the death animation, depending on the cause of death
     * @param s The current game state
     */
    animateDeath = (s: State) => {
        const froggy = s.frog,
            is_land = s.frog.deathDetails.deathBy === "collision",
            png_prefix = is_land ? "death_road_" : "death_water_"
        range(4).map((num) => {
            const deathImg = createImg(),
                deathAttr = Object.entries({
                    ...s.frog.deathDetails, width: froggy.width, height: froggy.height,
                    id: "death" + String(num), href: is_land || num != 3 ? "../assets/" + png_prefix + String(num) + ".png" : ""
                })
            attr(deathImg)(deathAttr)
            timer(CONS.STS.DTH_INV * num).subscribe(() => {
                svg.appendChild(deathImg)
                timer(CONS.STS.DTH_INV).subscribe(() => svg.removeChild(deathImg))
            })
        })
    }