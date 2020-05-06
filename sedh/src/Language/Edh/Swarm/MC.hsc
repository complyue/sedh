
#include <HsNet.h>

module Language.Edh.Swarm.MC where 

import Prelude

import Foreign
import Foreign.C.Types


_IP_MULTICAST_IF, _IP_MULTICAST_TTL, _IP_MULTICAST_LOOP, _IP_ADD_MEMBERSHIP, _IP_DROP_MEMBERSHIP :: CInt
_IP_MULTICAST_IF    = #const IP_MULTICAST_IF
_IP_MULTICAST_TTL   = #const IP_MULTICAST_TTL
_IP_MULTICAST_LOOP  = #const IP_MULTICAST_LOOP
_IP_ADD_MEMBERSHIP  = #const IP_ADD_MEMBERSHIP
_IP_DROP_MEMBERSHIP = #const IP_DROP_MEMBERSHIP

_IPPROTO_IP :: CInt
_IPPROTO_IP = #const IPPROTO_IP
