module Agents (Agent(..), isAgentType) where

import AgentTypes (AgentType)
import Components (Component)

data Agent = Agent { agentType :: AgentType, agentId :: Int, components :: [Component] }
instance Show Agent where
  show a = (show $ agentType a) ++ "[" ++ (show $ agentId a) ++ "]" ++ (show $ components a)

isAgentType :: AgentType -> Agent -> Bool
isAgentType t a = agentType a == t
