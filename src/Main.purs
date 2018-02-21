module Main where

import Control.Monad.Eff.Random
import Data.Int
import Data.Maybe
import Data.String
import FRP
import Game.GameTypes
import Game.GameVariables
import Prelude
import UI.Elements
import UI.Events
import UI.Properties
import Control.Alternative (pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Plus ((<|>))
import DOM (DOM)
import DOM.Event.Types (Event)
import Data.Array (concatMap, (..))
import Data.Traversable (for, traverse)
import Data.Unit (unit)
import FRP as F
import FRP.Event as E
import FRP.Event.Keyboard (down)
import FRP.Event.Time (animationFrame)
import Halogen.VDom (VDom)
import Neon.Operator ((%))
import UI.Core (MEvent, AttrValue(..), Attr(..), Prop)
import UI.Properties (width, height)
import UI.Util as U
foreign import click :: MEvent
foreign import change :: MEvent


imageObject ::forall a. String -> String-> Bounding ->  VDom Attr a
imageObject uniqId url bound =  imageView
                        [
                          id_ uniqId
                        , width (show bound.w)
                        , height (show bound.h)
                        , margin ((show bound.x) <> ","<>(show bound.y)<>",0,0")
                        , imageUrl url
                        ]


getBackground ::forall i. VDom Attr i
getBackground = imageView
                [
                  id_ "background"
                , width "match_parent"
                , height "match_parent"
                , imageUrl "background"
                ]

getScoreBoard :: forall i.StateType -> VDom Attr i
getScoreBoard state = relativeLayout
                      [
                        id_ "scoreBoard"
                      , width "200"
                      , height "100"
                      , margin "1020,170,0,0"
                      ]
                      [
                        (imageObject "wooden" "board" {x:50,y:20,w:200,h:100}),

                        textView
                        [
                          id_ "score"
                        , width "200"
                        , height "70"
                        , color "#ffffff"
                        , textSize "18"
                        , margin "70,55,0,0"
                        , text (show $ "Distance: "<> (show $ getScore state) <> "m")
                        , fontStyle "Rammetto One"
                        ]
                      ]

getGameStat :: forall i. StateType -> VDom Attr i
getGameStat state = relativeLayout
                    [
                      id_ "gameStats"
                    , width "500"
                    , height "200"
                    , centerInParent "true,-1"
                    , visibility ( if state.gameState 
                                            then "gone"
                                            else "visible")
                    ]
                    [
                      textView
                      [
                        id_ "gameState" 
                      , width "400"
                      , height "80"
                      , centerInParent "true,-1"
                      , fontStyle "Rammetto One"
                      , text "Game Over"
                      , textSize "45"
                      , color "#ffffff"
                      ]
                    ]

topPipes :: forall a. Eff (random :: RANDOM |a ) (Array Bounding)
topPipes = (traverse (\i -> randomInt 0 40 >>= \n-> pure { x:  1+(i * pipeGap) , y: 0, w: 60, h: (150 + n) }) (1..5))
 
bottomPipes :: forall a. Eff (random :: RANDOM |a ) (Array Bounding)
bottomPipes = (traverse (\i -> randomInt 0 40 >>= \n-> pure { x:  (i * pipeGap) , y: groundY-(150+n), w: 60, h: (150 + n) }) (1..5))


widget :: forall a.StateType -> VDom Attr a
widget state = linearLayout
                [  id_ "1"
                  , height "match_parent"
                  , width "match_parent"
                  , background "#8B4513"
                  , gravity "center"
                  , orientation "vertical"
                  , onClick (Some click)
                ]
                [
                  relativeLayout
                  [
                    id_ "2"
                  , height "650"
                  , width "match_parent"
                  , background "#ffffff"
                  ]
                  [
                    (getBackground),

                    imageView
                    [
                      id_ "ground"
                    , width "match_parent"
                    , height "183"
                    , imageUrl "flappy_ground"
                    , margin ((show state.ground.x) <> ","<>(show state.ground.y)<>",0,0") 
                    ],

                    (imageObject "flappy" "flappy" state.player),

                    relativeLayout
                    [
                      id_ "pipeHolder"
                    , width "match_parent"
                    , height "match_parent"
                    ]
                    (map (\o -> (imageObject ("id"<>show o.x) "pipe" o)) state.pipes),

                    (getScoreBoard state),

                    (getGameStat state)

                  ]
                    
               ]

resetGame ::forall a. Eff( random :: RANDOM| a) StateType
resetGame = do
  _ <- U.updateState "ground" {x:0,y:groundY,w:1200,h:500}
  _ <- U.updateState "player" {x:50,y:groundY-50,w:40,h:40}
  top <- topPipes
  bottom <- bottomPipes
  _ <- U.updateState "pipes" (top<>bottom)
  _ <- U.updateState "egravity" 0
  _ <- U.updateState "init" 5
  _ <- U.updateState "finalVelocity" 5
  _ <- U.updateState "currTime" 0
  _ <- U.updateState "jump" false
  _ <- U.updateState "timer" 0
  U.updateState "gameState" isAlive


initializeVel :: forall a.Eff (| a) StateType
initializeVel = do 
  _ <- U.updateState "jump" true
  _ <- U.updateState "initialVelocity" 5
  _ <- U.updateState "egravity" 1
  _ <- U.updateState "finalVelocity" 5
  U.updateState "currTime" 0


collDet :: Bounding -> Bounding -> Boolean
collDet r1 r2 = do 
           not ((r2.x > (r1.x+r1.w)) || 
               ((r2.x+r2.w) < r1.x)  || 
               (r2.y > (r1.y+r1.h))  ||
               ((r2.y+r2.h) < r1.y))


getScore :: StateType -> Int
getScore state = state.timer/500


updateFlappy :: forall a.StateType -> Eff (| a) StateType
updateFlappy (state::StateType) 
      | (state.gameState == isAlive) =do
          _ <- if (state.jump)
            then do
                state<- U.updateState "currTime" (state.currTime+20)
                if (state.finalVelocity==state.initialVelocity) && (state.finalVelocity>0)
                  then do
                    t<- U.updateState "egravity" (-1)
                    pure unit
                  else
                    pure unit

                state<-U.getState

                if state.finalVelocity==0
                  then do
                    w<-U.updateState "egravity" (-0.5)
                    w<-U.updateState "initialVelocity" 0
                    pure unit
                  else
                    pure unit

                state<-U.getState
                (state::StateType)<-U.updateState "finalVelocity" ((state.initialVelocity + (state.egravity*((state.currTime-startTime)/100))))
                U.updateState "player" (state.player{y=(state.player.y-state.finalVelocity)})

            else
              U.getState

          U.getState
      | otherwise = pure state
        

updatePipes :: forall a.StateType -> Eff (| a) StateType
updatePipes (state::StateType) 
      | (state.gameState == isAlive) = (U.updateState "pipes" (map (\o->o{x=((o.x+(if o.x < -100
                                                              then 1200
                                                              else 0
        ))-velocityX)}) state.pipes))
      | otherwise = pure state


updateTimer :: forall a.StateType -> Eff (| a) StateType
updateTimer (state::StateType) 
      | (state.gameState == isAlive) = (U.updateState "timer" (state.timer+20))
      | otherwise = pure state



main :: forall a.Eff( random :: RANDOM, console :: CONSOLE, dom :: DOM, frp :: FRP| a) Unit 
main = do
  U.initializeState
  state <- resetGame
  U.render (widget state) listen

        
eval :: forall a . Boolean -> Eff (| a) StateType
eval j = do
  (state::StateType)<-U.getState
  _<- (traverse (\o -> do
                  let c = collDet state.player o
                  if c
                    then  U.updateState "gameState" $ not isAlive
                    else  U.getState                  )
                 state.pipes)
  state<- updateTimer state
  state <- updatePipes state
  updateFlappy state

listen :: forall a.Eff( frp :: FRP, console :: CONSOLE| a)(Eff( frp :: FRP, console :: CONSOLE| a ) Unit)
listen = do
  jump <- U.signal "1" false
  _ <- jump.event `E.subscribe` (\_ -> initializeVel )
  _ <- down `E.subscribe` (\key -> void $ case key of
          32 -> initializeVel
          _ -> U.getState)

  let behavior = eval <$> jump.behavior 
  let events = (animationFrame)

  U.patch widget behavior events