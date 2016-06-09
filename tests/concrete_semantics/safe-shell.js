if (typeof version != 'undefined')
{
  version(0);
}

var STATUS = "STATUS: ";
var VERBOSE = false;
var SECT_PREFIX = 'Section ';
var SECT_SUFFIX = ' of test - ';

var gDelayTestDriverEnd = false;

var summary = '';
var description = '';
var expected = '';
var actual = '';
var msg = '';

var SECTION = "";
var VERSION = "";
var BUGNUMBER = "";

var SAFE_TESTS = 0

/*
 * constant strings
 */
var GLOBAL = this + '';
var PASSED = " PASSED! ";
var FAILED = " FAILED! ";

var DEBUG = false;

var DESCRIPTION;
var EXPECTED;

function TestCase(n, d, e, a)
{
  this.name = n;
  this.description = d;
  this.__expect1 = e;
  this.__result1 = a;
  this.reason = '';
  this.bugnumber = typeof(BUGNUMBER) != 'undefined' ? BUGNUMBER : '';
  this.type = 'safe-testobj';
}

function TestError(msg)
{
  this.reason = msg
  this.type = 'safe-errorobj';
}

// test262 $ERROR function
function $ERROR(msg) {
  new TestError(msg)
}

// test262 assert
function assert() {}
assert.sameValue = function (actual, expected, message) {
  new TestCase("name", message, expected, actual)
}

function startTest() {}
function test() {}
function writeHeaderToLog() {}
