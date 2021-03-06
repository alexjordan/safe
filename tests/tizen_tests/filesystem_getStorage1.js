/*******************************************************************************
    Copyright (c) 2013, S-Core.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ******************************************************************************/
var __result1, __result2, __result3;
function onStorage(storage) {
     __result1 = storage.label;
     __result2 = storage.type;
}

function onStorageError(e) {
     __result3 = e.name;
}

tizen.filesystem.getStorage("music", onStorage, onStorageError);


var __expect1 = "music";
var __expect2 = "INTERNAL";
var __expect3 = "UnknownError";