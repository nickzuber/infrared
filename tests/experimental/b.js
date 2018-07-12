'use strict';

// The current time.
const CURRENT_TIME = +Date.now();
const FAVORITE_NUMBER = 21;

function* handleLevelPairing (levelId, token, boardId, boardType, settings) {
  // TEMP WORKAROUND
  sessionStorage.setItem('token', token);

  yield put(PairingActions.setLevelPairing(levelId, token, boardId, boardType, settings));
  yield call(getLevelAncestry, levelId);
}
