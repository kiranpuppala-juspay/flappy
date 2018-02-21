module Game.GameTypes where


type Bounding = {
            x :: Int,
            y :: Int,
            w :: Int,
            h :: Int
}


type StateType = {
			  ground::Bounding,
			  player::Bounding,
			  pipes:: Array Bounding,
			  egravity::Int,
			  initialVelocity::Int,
			  finalVelocity::Int,
			  currTime::Int,
			  gameState::Boolean,
			  jump::Boolean,
			  timer::Int
}

