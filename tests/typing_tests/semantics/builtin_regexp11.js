/*******************************************************************************
    Copyright (c) 2013, KAIST.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ******************************************************************************/

var o_1 = new RegExp("^(?:\\s*(<[\\w\\W]+>)[^>]*|#([\\w-]*))$");
var o_2 = RegExp(o_1, undefined);
var o_3 = new RegExp(o_1, undefined)

var __result1 = o_1 == o_2;
var __expect1 = true;
var __result2 = o_1 == o_3;
var __expect2 = false;
