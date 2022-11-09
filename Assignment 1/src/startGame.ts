import "./style.css";
import { createImg, attr, range } from "./utilityFuncs"
import { CONS, svg } from "./consts";


/** The purpose of this function is to initialise every svg and image required throughout the game
 *  It is only called once in order to prevent redundant calls where the function has no effects
 *  
 *  The elements consists of:
 *      - Game background
 *      - Frog
 *      - "Score", "High Score" and "Lives" words
 *      - Svg and image for each digit of the score and high score
 */
export const startGame = (): void => {
    const screen = document.getElementById("entireScreen")!,
        gameBg = createImg(), froggy = createImg(), midFrog = createImg(), score = createImg(), high_score = createImg(), lives = createImg(),
        pause = createImg(), fly = createImg(), gameOver = createImg(), goLine1 = createImg(), goLine2 = createImg(),
        bgAttr = Object.entries({
            id: "gameBg", href: "../assets/bg.png", x: "0px", y: "0px", width: "540px", height: "598px"
        }),

        frogAttr = Object.entries({ id: "froggy", href: "../assets/frog_0.png" }),
        midFrogAttr = Object.entries({ id: "midFroggy", href: "../assets/frog_1.png", visibility: "hidden" }),

        scoreAttr = Object.entries({
            id: "score", href: "../assets/score.png", x: "0px", y: "0px", width: "69px", height: "12px"
        }),

        hsAttr = Object.entries({
            id: "highScore", href: "../assets/highscore.png", x: "180px", y: "0px", width: "130.625px", height: "12px"
        }),

        livesAttr = Object.entries({
            id: "lives", href: "../assets/lives.png", x: "0px", y: "562px", width: "59.368px", height: "12px"
        }),

        pauseAttr = Object.entries({
            id: "paused", href: "../assets/paused.png", x: "135px", y: "288.5px", width: "270px", height: "40px", visibility: "hidden"
        }),

        flyAttr = Object.entries({
            id: "fly", href: "../assets/fly.png", x: "43px", y: "68px", width: "23px", height: "20.5px", visibility: "hidden"
        }),

        gameOverAttr = Object.entries({
            id: "gameOver", href: "../assets/game_over.png", x: "157.5px", y: "182.5px", width: "225px", height: "233px", visibility: "hidden"
        }),

        goLine1Attr = Object.entries({
            id: "gameOverLine1", href: "../assets/game_over_line_1.png", x: "182.5px", y: "300px", width: "175px", height: "13px", visibility: "hidden"
        }),

        goLine2Attr = Object.entries({
            id: "gameOverLine2", href: "../assets/game_over_line_2.png", x: "197.25px", y: "335px", width: "145.5px", height: "15px", visibility: "hidden"
        }),

        bulletY = [190, 255, 345, 460],

        imgAttr: [Element, [string, string | number | boolean][]][] =
            [[gameBg, bgAttr], [froggy, frogAttr], [midFrog, midFrogAttr],
            [score, scoreAttr], [high_score, hsAttr], [lives, livesAttr], [pause, pauseAttr], [fly, flyAttr],
            [gameOver, gameOverAttr], [goLine1, goLine1Attr], [goLine2, goLine2Attr]],

        /**
         * Function to produce a list of key, value pairs of the winning frog attributes
         * @param goalNum The number of the goal
         * @returns A list of key, value pairs of the winning frog attributes
         */
        winFrogAttr = (goalNum: number) => Object.entries({
            id: "winFrog" + goalNum, href: "../assets/goal_frog_0.png",
            x: CONS.GOAL.GOAL_X + CONS.GOAL.GOAL_SPACING * goalNum, y: CONS.GOAL.GOAL_Y,
            width: "27", height: "32", visibility: "hidden"
        }),

        /**
         * Function to create entity elements and append them to the main svg canvas
         * @param eId A unique string to identify the element
         * @param png A string identifying which png to use
         */
        createEntities = (eId: string, png: string): void => {
            const v = createImg(),
                vAttr = Object.entries({ id: eId, href: "../assets/" + png + ".png" })
            attr(v)(vAttr), svg.appendChild(v)
            if (png === "gator_0") attr(v)([["height", "33px"]])
        },

        /** Function to create a nested svg element and an image element inside the svg element. 
         *      Each svg represents one digit of the current score and high score
         *  @param dgtNum the digit "column" number
         */
        createDgtSvg = (dgtNum: number): void => {
            const score_svg = document.createElementNS(svg.namespaceURI, "svg"), score_num = createImg(),
                hs_svg = document.createElementNS(svg.namespaceURI, "svg"), hs_num = createImg(),
                svgAttr = (highScore: boolean): [string, string][] => {
                    const svgId = highScore ? "hsSvg" : "scoreSvg",
                        xOffset = highScore ? 180 : 0
                    return Object.entries({
                        id: svgId + String(dgtNum), viewBox: "-1 -4.5 90 100", x: String(13 * dgtNum + xOffset), y: "-10"
                    })
                },

                numAttr = (highScore: boolean): [string, string][] => {
                    const imgId = highScore ? "hsNum" : "scoreNum"
                    return Object.entries({
                        id: imgId + String(dgtNum), href: "../assets/font_numbers.png", x: "0", y: "0",
                        width: "0", height: "2", preserveAspectRatio: "xMinYMin slice"
                    })
                }
            attr(score_svg)(svgAttr(false)), attr(score_num)(numAttr(false)), attr(hs_svg)(svgAttr(true)), attr(hs_num)(numAttr(true))
            svg.appendChild(score_svg), score_svg.appendChild(score_num), svg.appendChild(hs_svg), hs_svg.appendChild(hs_num)
        },

        lifeFrogAttr = (lifeNum: number) => Object.entries({
            id: "lifeFrog" + String(lifeNum), href: "../assets/life.png", x: String(lifeNum * 15) + "px", y: "578px", width: "14.25", height: "13.458px", visibility: "hidden"
        }),

        showBullet = (bNum: number) => {
            const bullet = createImg(),
                bullAttr = {
                    id: "blt" + bNum, href: "../assets/bullet_point.png",
                    width: "15px", height: "15px",
                    x: "945px", y: bulletY[bNum]
                }
            attr(bullet)(Object.entries(bullAttr))
            screen.appendChild(bullet)
        }

    // For each of the individual elements, we assign their attributes and add them to svg
    imgAttr.forEach(([img, key_val]) => { attr(img)(key_val); svg.appendChild(img) })

    // Initialising the svg/images for score, high score and the entities (car, logs, turtles)
    range(CONS.FIVE).forEach(row => {
        range(CONS.LAN.AMT[row]).forEach(col => { createEntities("veh" + row + col, "vehicle_" + row) });
        range(CONS.RIV.AMT[row]).forEach(col => {
            createEntities("riv" + row + col,
                row === CONS.ENT.GTR_ROW && col === CONS.ENT.GTR_COL ? "gator_0" : "river_" + row)
        });
        createDgtSvg(row)

        const wFrog = createImg()
        attr(wFrog)(winFrogAttr(row))
        svg.appendChild(wFrog)
    })

    range(3).forEach(num => {
        const lifeFrog = createImg()
        attr(lifeFrog)(lifeFrogAttr(num)), svg.appendChild(lifeFrog)
    })


    // Logo and Instructions inserted here
    const
        instImg = createImg(), logoLeft = createImg(), logoRight = createImg(), obstacles = createImg(),
        gator = createImg(), g_desc_0 = createImg(), g_desc_1 = createImg(),
        log = createImg(), l_desc_0 = createImg(), l_desc_1 = createImg(),
        turt = createImg(), t_desc_0 = createImg(), t_desc_1 = createImg(),
        car = createImg(), c_desc_0 = createImg(), c_desc_1 = createImg(),

        obstaclesX = "40px", obstacleHgt = "12px",

        instImgAttr = Object.entries({
            id: "instructions", href: "../assets/instructions.png", x: "940px", y: "100px", width: "350px", height: "26px"
        }),

        logoLeftAttr = Object.entries({
            id: "logo", href: "../assets/title.png", x: "50px", y: "20px", width: "285px", height: "38px"
        }),

        logoRightAttr = Object.entries({
            id: "logo", href: "../assets/title.png", x: "955px", y: "20px", width: "285px", height: "38px"
        }),

        obsAttr = Object.entries({
            id: "obstacles", href: "../assets/obstacles.png", x: "50px", y: "100px", width: "263px", height: "26px"
        }),

        gatorAttr = Object.entries({
            id: "gator", href: "../assets/gator.png", x: obstaclesX, y: "180px", width: "67px", height: obstacleHgt
        }),

        gDesc0Attr = Object.entries({
            id: "g_desc_0", href: "../assets/gator_desc_0.png", x: obstaclesX, y: "200px", width: "216px", height: obstacleHgt
        }),

        gDesc1Attr = Object.entries({
            id: "g_desc_1", href: "../assets/gator_desc_1.png", x: obstaclesX, y: "220px", width: "271px", height: obstacleHgt
        }),

        logAttr = Object.entries({
            id: "log", href: "../assets/log.png", x: obstaclesX, y: "280px", width: "37px", height: obstacleHgt
        }),

        lDesc0Attr = Object.entries({
            id: "l_desc_0", href: "../assets/log_desc_0.png", x: obstaclesX, y: "300px", width: "79.5px", height: obstacleHgt
        }),

        lDesc1attr = Object.entries({
            id: "l_desc_1", href: "../assets/log_desc_1.png", x: obstaclesX, y: "320px", width: "225px", height: obstacleHgt
        }),

        turtAttr = Object.entries({
            id: "turt", href: "../assets/turt.png", x: obstaclesX, y: "380px", width: "79px", height: obstacleHgt
        }),

        tDesc0Attr = Object.entries({
            id: "t_desc_0", href: "../assets/turt_desc_0.png", x: obstaclesX, y: "400px", width: "228px", height: obstacleHgt
        }),

        tDesc1Attr = Object.entries({
            id: "t_desc_1", href: "../assets/turt_desc_1.png", x: obstaclesX, y: "420px", width: "273px", height: obstacleHgt
        }),

        carAttr = Object.entries({
            id: "car", href: "../assets/car.png", x: obstaclesX, y: "480px", width: "39px", height: obstacleHgt
        }),

        cDesc0Attr = Object.entries({
            id: "c_desc_0", href: "../assets/car_desc_0.png", x: obstaclesX, y: "500px", width: "235px", height: obstacleHgt
        }),

        cDesc1Attr = Object.entries({
            id: "c_desc_1", href: "../assets/car_desc_1.png", x: obstaclesX, y: "520px", width: "262px", height: obstacleHgt
        }),

        instId = "instructions_line_", instHref = "../assets/" + instId, instHgt = 16,
        inst = { width: [282, 282, 282, 326, 238, 300, 302, 250, 250, 286, 286], y: [190, 215, 255, 280, 305, 345, 370, 395, 420, 460, 485] },

        screenAttr: [Element, [string, string | number | boolean][]][] =
            [[instImg, instImgAttr], [logoLeft, logoLeftAttr], [logoRight, logoRightAttr], [obstacles, obsAttr],
            [gator, gatorAttr], [g_desc_0, gDesc0Attr], [g_desc_1, gDesc1Attr],
            [log, logAttr], [l_desc_0, lDesc0Attr], [l_desc_1, lDesc1attr],
            [turt, turtAttr], [t_desc_0, tDesc0Attr], [t_desc_1, tDesc1Attr],
            [car, carAttr], [c_desc_0, cDesc0Attr], [c_desc_1, cDesc1Attr]],

        showInst = (lineNum: number) => {
            const instLine = createImg(),
                instAttr = {
                    id: instId + String(lineNum), href: instHref + String(lineNum) + ".png",
                    width: inst.width[lineNum - 1], height: instHgt,
                    x: lineNum === 8 ? "955px" : "970px", y: inst.y[lineNum - 1]
                }
            attr(instLine)(Object.entries(instAttr))
            screen.appendChild(instLine)
        }
    range(4).forEach(num => showBullet(num))
    range(11).forEach(num => showInst(num + 1))
    screenAttr.forEach(([img, key_val]) => { attr(img)(key_val); screen.appendChild(img) })
    attr(instImg)(instImgAttr)
    screen.appendChild(instImg)
}