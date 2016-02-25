if (typeof version != 'undefined')
{
  version(0);
}

var STATUS = "STATUS: ";
var VERBOSE = false;
var SECT_PREFIX = 'Section ';
var SECT_SUFFIX = ' of test - ';

var gDelayTestDriverEnd = false;

var BUGNUMBER = '';
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
  this.bugnumber = typeof(BUGNUMER) != 'undefined' ? BUGNUMBER : '';
  this.type = 'safe-shell';
}

function startTest() {}
function test() {}
function writeHeaderToLog() {}
