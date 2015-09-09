{-# LANGUAGE TupleSections #-}

module Borel.Ceilometer.Instance where

import           Ceilometer.Types

billableStatus :: PFInstanceStatus -> Bool
billableStatus InstanceError            = False
billableStatus InstanceActive           = True
billableStatus InstanceShutoff          = False
billableStatus InstanceBuild            = False
billableStatus InstanceRebuild          = False
billableStatus InstanceDeleted          = False
billableStatus InstanceSoftDeleted      = False
billableStatus InstanceShelved          = False
billableStatus InstanceShelvedOffloaded = False
billableStatus InstanceReboot           = True
billableStatus InstanceHardReboot       = True
billableStatus InstancePassword         = True
billableStatus InstanceResize           = True
billableStatus InstanceVerifyResize     = True
billableStatus InstanceRevertResize     = True
billableStatus InstancePaused           = False
billableStatus InstanceSuspended        = False
billableStatus InstanceRescue           = False
billableStatus InstanceMigrating        = True
