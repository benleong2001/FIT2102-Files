import "./style.css";

/**
   * TYPES ARE HERE!!!
   */
type Direction = "vertical" | "horizontal"
type Event = "keyup" | "keydown"
// type River = River
type Entity = Car | River
type Key = "w" | "a" | "s" | "d" | "arrowup" | "arrowdown" | "arrowright" | "arrowleft" | "r" | "p"

type Vec = Readonly<{
    x: number,
    y: number,
    row: number
    rotate: number,
}>

type Frog = Readonly<{
    id: string, row: number,
    x: number, y: number, vel: number,
    width: number, height: number,
    rotate: number, moving: boolean,
    collided: boolean, deathDetails: {
        deathBy: string | null,
        x: number, y: number
    },
}>

type Car = Readonly<{
    id: string,
    x: number, y: number, vel: number,
    width: number, height: number,
    offset: number, 
}>

type River = Readonly<{
    id: string,
    x: number, y: number,
    vel: number,
    width: number, height: number,
    sinking: boolean, sunk: boolean,
    offset: number, isGator: boolean
}>

type Fly = Readonly<{
    appear: boolean, goalNum: number, rng: Rng,
    x: number, y: number, width: number, height: number, 
}>

type Rng = Readonly<{value: number, next(): Rng}>

type State = Readonly<{
    gametime: number,
    level: number,

    frog: Frog,
    carRows: Car[][],
    riverRows: River[][],
    fly: Fly,

    targets: boolean[],
    highScore: number,
    highestRow: number,
    score: number, 
    lives: number, 
    
    gameOver: boolean,
    paused: boolean,
    reset: boolean
}>
export { Direction, Entity, River, Event, Key, Vec, Frog, Car, State, Fly };