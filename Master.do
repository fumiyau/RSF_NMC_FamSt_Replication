clear all
set more off

global ROOT "`c(pwd)'"
mkdir "${ROOT}/Data"
mkdir "${ROOT}/Data/NVSS"
mkdir "${ROOT}/Data/CPS-ASEC"

do "${ROOT}/1.NVSS_DataConst.do"
do "${ROOT}/2.NVSS_RSF_Revision.do"

do "${ROOT}/5.CPS_AECS_RSF_Revision.do"

global SSM_DIR "${ROOT}/Data/SSM"
global JGSS_DIR "${ROOT}/Data/JGSS"
global Data_DIR "${ROOT}/Data"
global Tables_DIR "${ROOT}/Tables"

do "${ROOT}/Replication/1.SSM.do"
do "${ROOT}/Replication/2.JGSS.do"
do "${ROOT}/Replication/3.Reshape_SSM.do"
do "${ROOT}/Replication/4.Desc.do"
