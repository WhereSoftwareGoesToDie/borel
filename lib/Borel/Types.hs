-- * Types for entities that a Borel query needs to know about,
--   which includes resources (e.g. cpu, memory in openstack),
--   customers, metrics
--
module Borel.Types
  ( module Borel.Types.Core
  , module Borel.Types.Resource
  ) where

import Borel.Types.Core
import Borel.Types.Resource
