module Agents (Agent(..), isAgentType, getComponents, setComponents) where

import AgentTypes (AgentType)
import Components (Component)

data Agent = Agent { agentType :: AgentType, agentId :: Int, components :: [Component] }

instance Eq Agent where
  x == y = (agentId x) == (agentId y)

instance Show Agent where
  show a = (show $ agentType a) ++ "[" ++ (show $ agentId a) ++ "]" ++ (show $ components a)

isAgentType :: AgentType -> Agent -> Bool
isAgentType t a = agentType a == t

getComponents :: Agent -> [Component]
getComponents ag = components ag

setComponents :: Agent -> [Component] -> Agent
setComponents ag cs = ag { components = cs }
