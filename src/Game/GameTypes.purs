module Game.GameTypes where


type GameObject = {
            x :: Int,
            y :: Int,
            w :: Int,
            h :: Int,
						sprite::String
}


type StateType = {
			  ground::GameObject,
			  player::GameObject,
			  pipes:: Array GameObject,
			  egravity::Int,
			  initialVelocity::Int,
			  finalVelocity::Int,
			  currTime::Int,
			  gameState::Boolean,
			  jump::Boolean,
			  timer::Int
}

