module AgentTypes (AgentType(..)) where

data AgentType
  = Prey
  | Hunter
  | Food
  deriving (Eq, Show)
