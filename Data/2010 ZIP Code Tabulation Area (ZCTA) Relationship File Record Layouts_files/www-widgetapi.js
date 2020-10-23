(function(){/*

 Copyright The Closure Library Authors.
 SPDX-License-Identifier: Apache-2.0
*/
var p;function aa(a){var b=0;return function(){return b<a.length?{done:!1,value:a[b++]}:{done:!0}}}
var ba="function"==typeof Object.defineProperties?Object.defineProperty:function(a,b,c){if(a==Array.prototype||a==Object.prototype)return a;a[b]=c.value;return a};
function ca(a){a=["object"==typeof globalThis&&globalThis,a,"object"==typeof window&&window,"object"==typeof self&&self,"object"==typeof global&&global];for(var b=0;b<a.length;++b){var c=a[b];if(c&&c.Math==Math)return c}throw Error("Cannot find global object");}
var da=ca(this);function t(a,b){if(b)a:{for(var c=da,d=a.split("."),e=0;e<d.length-1;e++){var f=d[e];if(!(f in c))break a;c=c[f]}d=d[d.length-1];e=c[d];f=b(e);f!=e&&null!=f&&ba(c,d,{configurable:!0,writable:!0,value:f})}}
t("Symbol",function(a){function b(e){if(this instanceof b)throw new TypeError("Symbol is not a constructor");return new c("jscomp_symbol_"+(e||"")+"_"+d++,e)}
function c(e,f){this.f=e;ba(this,"description",{configurable:!0,writable:!0,value:f})}
if(a)return a;c.prototype.toString=function(){return this.f};
var d=0;return b});
t("Symbol.iterator",function(a){if(a)return a;a=Symbol("Symbol.iterator");for(var b="Array Int8Array Uint8Array Uint8ClampedArray Int16Array Uint16Array Int32Array Uint32Array Float32Array Float64Array".split(" "),c=0;c<b.length;c++){var d=da[b[c]];"function"===typeof d&&"function"!=typeof d.prototype[a]&&ba(d.prototype,a,{configurable:!0,writable:!0,value:function(){return ha(aa(this))}})}return a});
function ha(a){a={next:a};a[Symbol.iterator]=function(){return this};
return a}
function u(a){var b="undefined"!=typeof Symbol&&Symbol.iterator&&a[Symbol.iterator];return b?b.call(a):{next:aa(a)}}
function ia(a){for(var b,c=[];!(b=a.next()).done;)c.push(b.value);return c}
var ja="function"==typeof Object.create?Object.create:function(a){function b(){}
b.prototype=a;return new b},la;
if("function"==typeof Object.setPrototypeOf)la=Object.setPrototypeOf;else{var ma;a:{var na={a:!0},oa={};try{oa.__proto__=na;ma=oa.a;break a}catch(a){}ma=!1}la=ma?function(a,b){a.__proto__=b;if(a.__proto__!==b)throw new TypeError(a+" is not extensible");return a}:null}var pa=la;
function sa(a,b){a.prototype=ja(b.prototype);a.prototype.constructor=a;if(pa)pa(a,b);else for(var c in b)if("prototype"!=c)if(Object.defineProperties){var d=Object.getOwnPropertyDescriptor(b,c);d&&Object.defineProperty(a,c,d)}else a[c]=b[c];a.F=b.prototype}
function ta(){this.m=!1;this.i=null;this.g=void 0;this.f=1;this.j=this.l=0;this.v=this.h=null}
function ua(a){if(a.m)throw new TypeError("Generator is already running");a.m=!0}
ta.prototype.s=function(a){this.g=a};
function va(a,b){a.h={fa:b,U:!0};a.f=a.l||a.j}
ta.prototype["return"]=function(a){this.h={"return":a};this.f=this.j};
function x(a,b,c){a.f=c;return{value:b}}
ta.prototype.B=function(a){this.f=a};
function wa(a){a.l=2;a.j=3}
function xa(a){a.l=0;a.h=null}
function ya(a){a.v=[a.h];a.l=0;a.j=0}
function za(a){var b=a.v.splice(0)[0];(b=a.h=a.h||b)?b.U?a.f=a.l||a.j:void 0!=b.B&&a.j<b.B?(a.f=b.B,a.h=null):a.f=a.j:a.f=0}
function Ea(a){this.f=new ta;this.g=a}
function Fa(a,b){ua(a.f);var c=a.f.i;if(c)return Ga(a,"return"in c?c["return"]:function(d){return{value:d,done:!0}},b,a.f["return"]);
a.f["return"](b);return Ha(a)}
function Ga(a,b,c,d){try{var e=b.call(a.f.i,c);if(!(e instanceof Object))throw new TypeError("Iterator result "+e+" is not an object");if(!e.done)return a.f.m=!1,e;var f=e.value}catch(g){return a.f.i=null,va(a.f,g),Ha(a)}a.f.i=null;d.call(a.f,f);return Ha(a)}
function Ha(a){for(;a.f.f;)try{var b=a.g(a.f);if(b)return a.f.m=!1,{value:b.value,done:!1}}catch(c){a.f.g=void 0,va(a.f,c)}a.f.m=!1;if(a.f.h){b=a.f.h;a.f.h=null;if(b.U)throw b.fa;return{value:b["return"],done:!0}}return{value:void 0,done:!0}}
function Ka(a){this.next=function(b){ua(a.f);a.f.i?b=Ga(a,a.f.i.next,b,a.f.s):(a.f.s(b),b=Ha(a));return b};
this["throw"]=function(b){ua(a.f);a.f.i?b=Ga(a,a.f.i["throw"],b,a.f.s):(va(a.f,b),b=Ha(a));return b};
this["return"]=function(b){return Fa(a,b)};
this[Symbol.iterator]=function(){return this}}
function y(a,b){var c=new Ka(new Ea(b));pa&&a.prototype&&pa(c,a.prototype);return c}
t("Reflect.setPrototypeOf",function(a){return a?a:pa?function(b,c){try{return pa(b,c),!0}catch(d){return!1}}:null});
t("Object.setPrototypeOf",function(a){return a||pa});
function z(a,b){return Object.prototype.hasOwnProperty.call(a,b)}
var La="function"==typeof Object.assign?Object.assign:function(a,b){for(var c=1;c<arguments.length;c++){var d=arguments[c];if(d)for(var e in d)z(d,e)&&(a[e]=d[e])}return a};
t("Object.assign",function(a){return a||La});
t("Promise",function(a){function b(g){this.g=0;this.h=void 0;this.f=[];var h=this.i();try{g(h.resolve,h.reject)}catch(k){h.reject(k)}}
function c(){this.f=null}
function d(g){return g instanceof b?g:new b(function(h){h(g)})}
if(a)return a;c.prototype.g=function(g){if(null==this.f){this.f=[];var h=this;this.h(function(){h.j()})}this.f.push(g)};
var e=da.setTimeout;c.prototype.h=function(g){e(g,0)};
c.prototype.j=function(){for(;this.f&&this.f.length;){var g=this.f;this.f=[];for(var h=0;h<g.length;++h){var k=g[h];g[h]=null;try{k()}catch(l){this.i(l)}}}this.f=null};
c.prototype.i=function(g){this.h(function(){throw g;})};
b.prototype.i=function(){function g(l){return function(m){k||(k=!0,l.call(h,m))}}
var h=this,k=!1;return{resolve:g(this.G),reject:g(this.j)}};
b.prototype.G=function(g){if(g===this)this.j(new TypeError("A Promise cannot resolve to itself"));else if(g instanceof b)this.pa(g);else{a:switch(typeof g){case "object":var h=null!=g;break a;case "function":h=!0;break a;default:h=!1}h?this.v(g):this.l(g)}};
b.prototype.v=function(g){var h=void 0;try{h=g.then}catch(k){this.j(k);return}"function"==typeof h?this.da(h,g):this.l(g)};
b.prototype.j=function(g){this.m(2,g)};
b.prototype.l=function(g){this.m(1,g)};
b.prototype.m=function(g,h){if(0!=this.g)throw Error("Cannot settle("+g+", "+h+"): Promise already settled in state"+this.g);this.g=g;this.h=h;this.s()};
b.prototype.s=function(){if(null!=this.f){for(var g=0;g<this.f.length;++g)f.g(this.f[g]);this.f=null}};
var f=new c;b.prototype.pa=function(g){var h=this.i();g.H(h.resolve,h.reject)};
b.prototype.da=function(g,h){var k=this.i();try{g.call(h,k.resolve,k.reject)}catch(l){k.reject(l)}};
b.prototype.then=function(g,h){function k(r,q){return"function"==typeof r?function(v){try{l(r(v))}catch(w){m(w)}}:q}
var l,m,n=new b(function(r,q){l=r;m=q});
this.H(k(g,l),k(h,m));return n};
b.prototype["catch"]=function(g){return this.then(void 0,g)};
b.prototype.H=function(g,h){function k(){switch(l.g){case 1:g(l.h);break;case 2:h(l.h);break;default:throw Error("Unexpected state: "+l.g);}}
var l=this;null==this.f?f.g(k):this.f.push(k)};
b.resolve=d;b.reject=function(g){return new b(function(h,k){k(g)})};
b.race=function(g){return new b(function(h,k){for(var l=u(g),m=l.next();!m.done;m=l.next())d(m.value).H(h,k)})};
b.all=function(g){var h=u(g),k=h.next();return k.done?d([]):new b(function(l,m){function n(v){return function(w){r[v]=w;q--;0==q&&l(r)}}
var r=[],q=0;do r.push(void 0),q++,d(k.value).H(n(r.length-1),m),k=h.next();while(!k.done)})};
return b});
function Ma(a,b,c){if(null==a)throw new TypeError("The 'this' value for String.prototype."+c+" must not be null or undefined");if(b instanceof RegExp)throw new TypeError("First argument to String.prototype."+c+" must not be a regular expression");return a+""}
t("String.prototype.endsWith",function(a){return a?a:function(b,c){var d=Ma(this,b,"endsWith");b+="";void 0===c&&(c=d.length);for(var e=Math.max(0,Math.min(c|0,d.length)),f=b.length;0<f&&0<e;)if(d[--e]!=b[--f])return!1;return 0>=f}});
t("String.prototype.startsWith",function(a){return a?a:function(b,c){var d=Ma(this,b,"startsWith");b+="";for(var e=d.length,f=b.length,g=Math.max(0,Math.min(c|0,d.length)),h=0;h<f&&g<e;)if(d[g++]!=b[h++])return!1;return h>=f}});
function Na(a,b){a instanceof String&&(a+="");var c=0,d=!1,e={next:function(){if(!d&&c<a.length){var f=c++;return{value:b(f,a[f]),done:!1}}d=!0;return{done:!0,value:void 0}}};
e[Symbol.iterator]=function(){return e};
return e}
t("Array.prototype.keys",function(a){return a?a:function(){return Na(this,function(b){return b})}});
t("Array.prototype.values",function(a){return a?a:function(){return Na(this,function(b,c){return c})}});
t("Object.is",function(a){return a?a:function(b,c){return b===c?0!==b||1/b===1/c:b!==b&&c!==c}});
t("Array.prototype.includes",function(a){return a?a:function(b,c){var d=this;d instanceof String&&(d=String(d));var e=d.length,f=c||0;for(0>f&&(f=Math.max(f+e,0));f<e;f++){var g=d[f];if(g===b||Object.is(g,b))return!0}return!1}});
t("String.prototype.includes",function(a){return a?a:function(b,c){return-1!==Ma(this,b,"includes").indexOf(b,c||0)}});
t("Array.prototype.entries",function(a){return a?a:function(){return Na(this,function(b,c){return[b,c]})}});
t("WeakMap",function(a){function b(k){this.f=(h+=Math.random()+1).toString();if(k){k=u(k);for(var l;!(l=k.next()).done;)l=l.value,this.set(l[0],l[1])}}
function c(){}
function d(k){var l=typeof k;return"object"===l&&null!==k||"function"===l}
function e(k){if(!z(k,g)){var l=new c;ba(k,g,{value:l})}}
function f(k){var l=Object[k];l&&(Object[k]=function(m){if(m instanceof c)return m;Object.isExtensible(m)&&e(m);return l(m)})}
if(function(){if(!a||!Object.seal)return!1;try{var k=Object.seal({}),l=Object.seal({}),m=new a([[k,2],[l,3]]);if(2!=m.get(k)||3!=m.get(l))return!1;m["delete"](k);m.set(l,4);return!m.has(k)&&4==m.get(l)}catch(n){return!1}}())return a;
var g="$jscomp_hidden_"+Math.random();f("freeze");f("preventExtensions");f("seal");var h=0;b.prototype.set=function(k,l){if(!d(k))throw Error("Invalid WeakMap key");e(k);if(!z(k,g))throw Error("WeakMap key fail: "+k);k[g][this.f]=l;return this};
b.prototype.get=function(k){return d(k)&&z(k,g)?k[g][this.f]:void 0};
b.prototype.has=function(k){return d(k)&&z(k,g)&&z(k[g],this.f)};
b.prototype["delete"]=function(k){return d(k)&&z(k,g)&&z(k[g],this.f)?delete k[g][this.f]:!1};
return b});
t("Map",function(a){function b(){var h={};return h.previous=h.next=h.head=h}
function c(h,k){var l=h.f;return ha(function(){if(l){for(;l.head!=h.f;)l=l.previous;for(;l.next!=l.head;)return l=l.next,{done:!1,value:k(l)};l=null}return{done:!0,value:void 0}})}
function d(h,k){var l=k&&typeof k;"object"==l||"function"==l?f.has(k)?l=f.get(k):(l=""+ ++g,f.set(k,l)):l="p_"+k;var m=h.g[l];if(m&&z(h.g,l))for(var n=0;n<m.length;n++){var r=m[n];if(k!==k&&r.key!==r.key||k===r.key)return{id:l,list:m,index:n,o:r}}return{id:l,list:m,index:-1,o:void 0}}
function e(h){this.g={};this.f=b();this.size=0;if(h){h=u(h);for(var k;!(k=h.next()).done;)k=k.value,this.set(k[0],k[1])}}
if(function(){if(!a||"function"!=typeof a||!a.prototype.entries||"function"!=typeof Object.seal)return!1;try{var h=Object.seal({x:4}),k=new a(u([[h,"s"]]));if("s"!=k.get(h)||1!=k.size||k.get({x:4})||k.set({x:4},"t")!=k||2!=k.size)return!1;var l=k.entries(),m=l.next();if(m.done||m.value[0]!=h||"s"!=m.value[1])return!1;m=l.next();return m.done||4!=m.value[0].x||"t"!=m.value[1]||!l.next().done?!1:!0}catch(n){return!1}}())return a;
var f=new WeakMap;e.prototype.set=function(h,k){h=0===h?0:h;var l=d(this,h);l.list||(l.list=this.g[l.id]=[]);l.o?l.o.value=k:(l.o={next:this.f,previous:this.f.previous,head:this.f,key:h,value:k},l.list.push(l.o),this.f.previous.next=l.o,this.f.previous=l.o,this.size++);return this};
e.prototype["delete"]=function(h){h=d(this,h);return h.o&&h.list?(h.list.splice(h.index,1),h.list.length||delete this.g[h.id],h.o.previous.next=h.o.next,h.o.next.previous=h.o.previous,h.o.head=null,this.size--,!0):!1};
e.prototype.clear=function(){this.g={};this.f=this.f.previous=b();this.size=0};
e.prototype.has=function(h){return!!d(this,h).o};
e.prototype.get=function(h){return(h=d(this,h).o)&&h.value};
e.prototype.entries=function(){return c(this,function(h){return[h.key,h.value]})};
e.prototype.keys=function(){return c(this,function(h){return h.key})};
e.prototype.values=function(){return c(this,function(h){return h.value})};
e.prototype.forEach=function(h,k){for(var l=this.entries(),m;!(m=l.next()).done;)m=m.value,h.call(k,m[1],m[0],this)};
e.prototype[Symbol.iterator]=e.prototype.entries;var g=0;return e});
t("Object.entries",function(a){return a?a:function(b){var c=[],d;for(d in b)z(b,d)&&c.push([d,b[d]]);return c}});
t("Set",function(a){function b(c){this.f=new Map;if(c){c=u(c);for(var d;!(d=c.next()).done;)this.add(d.value)}this.size=this.f.size}
if(function(){if(!a||"function"!=typeof a||!a.prototype.entries||"function"!=typeof Object.seal)return!1;try{var c=Object.seal({x:4}),d=new a(u([c]));if(!d.has(c)||1!=d.size||d.add(c)!=d||1!=d.size||d.add({x:4})!=d||2!=d.size)return!1;var e=d.entries(),f=e.next();if(f.done||f.value[0]!=c||f.value[1]!=c)return!1;f=e.next();return f.done||f.value[0]==c||4!=f.value[0].x||f.value[1]!=f.value[0]?!1:e.next().done}catch(g){return!1}}())return a;
b.prototype.add=function(c){c=0===c?0:c;this.f.set(c,c);this.size=this.f.size;return this};
b.prototype["delete"]=function(c){c=this.f["delete"](c);this.size=this.f.size;return c};
b.prototype.clear=function(){this.f.clear();this.size=0};
b.prototype.has=function(c){return this.f.has(c)};
b.prototype.entries=function(){return this.f.entries()};
b.prototype.values=function(){return this.f.values()};
b.prototype.keys=b.prototype.values;b.prototype[Symbol.iterator]=b.prototype.values;b.prototype.forEach=function(c,d){var e=this;this.f.forEach(function(f){return c.call(d,f,f,e)})};
return b});
var A=this||self;function B(a,b){for(var c=a.split("."),d=b||A,e=0;e<c.length;e++)if(d=d[c[e]],null==d)return null;return d}
function Oa(){}
function Pa(a){var b=typeof a;return"object"!=b?b:a?Array.isArray(a)?"array":b:"null"}
function Qa(a){var b=Pa(a);return"array"==b||"object"==b&&"number"==typeof a.length}
function Sa(a){return"function"==Pa(a)}
function Ta(a){var b=typeof a;return"object"==b&&null!=a||"function"==b}
function Ua(a){return Object.prototype.hasOwnProperty.call(a,Va)&&a[Va]||(a[Va]=++Wa)}
var Va="closure_uid_"+(1E9*Math.random()>>>0),Wa=0;function Xa(a,b,c){return a.call.apply(a.bind,arguments)}
function Ya(a,b,c){if(!a)throw Error();if(2<arguments.length){var d=Array.prototype.slice.call(arguments,2);return function(){var e=Array.prototype.slice.call(arguments);Array.prototype.unshift.apply(e,d);return a.apply(b,e)}}return function(){return a.apply(b,arguments)}}
function Za(a,b,c){Function.prototype.bind&&-1!=Function.prototype.bind.toString().indexOf("native code")?Za=Xa:Za=Ya;return Za.apply(null,arguments)}
var C=Date.now;function D(a,b){var c=a.split("."),d=A;c[0]in d||"undefined"==typeof d.execScript||d.execScript("var "+c[0]);for(var e;c.length&&(e=c.shift());)c.length||void 0===b?d[e]&&d[e]!==Object.prototype[e]?d=d[e]:d=d[e]={}:d[e]=b}
function E(a,b){function c(){}
c.prototype=b.prototype;a.F=b.prototype;a.prototype=new c;a.prototype.constructor=a}
function $a(a){return a}
;function F(a,b){var c=void 0;return new (c||(c=Promise))(function(d,e){function f(k){try{h(b.next(k))}catch(l){e(l)}}
function g(k){try{h(b["throw"](k))}catch(l){e(l)}}
function h(k){k.done?d(k.value):(new c(function(l){l(k.value)})).then(f,g)}
h((b=b.apply(a,void 0)).next())})}
;function ab(a){if(Error.captureStackTrace)Error.captureStackTrace(this,ab);else{var b=Error().stack;b&&(this.stack=b)}a&&(this.message=String(a))}
E(ab,Error);ab.prototype.name="CustomError";var bb=Array.prototype.indexOf?function(a,b){return Array.prototype.indexOf.call(a,b,void 0)}:function(a,b){if("string"===typeof a)return"string"!==typeof b||1!=b.length?-1:a.indexOf(b,0);
for(var c=0;c<a.length;c++)if(c in a&&a[c]===b)return c;return-1},H=Array.prototype.forEach?function(a,b,c){Array.prototype.forEach.call(a,b,c)}:function(a,b,c){for(var d=a.length,e="string"===typeof a?a.split(""):a,f=0;f<d;f++)f in e&&b.call(c,e[f],f,a)},hb=Array.prototype.reduce?function(a,b,c){return Array.prototype.reduce.call(a,b,c)}:function(a,b,c){var d=c;
H(a,function(e,f){d=b.call(void 0,d,e,f,a)});
return d};
function ib(a,b){a:{var c=a.length;for(var d="string"===typeof a?a.split(""):a,e=0;e<c;e++)if(e in d&&b.call(void 0,d[e],e,a)){c=e;break a}c=-1}return 0>c?null:"string"===typeof a?a.charAt(c):a[c]}
function jb(a){return Array.prototype.concat.apply([],arguments)}
function kb(a){var b=a.length;if(0<b){for(var c=Array(b),d=0;d<b;d++)c[d]=a[d];return c}return[]}
function lb(a,b){for(var c=1;c<arguments.length;c++){var d=arguments[c];if(Qa(d)){var e=a.length||0,f=d.length||0;a.length=e+f;for(var g=0;g<f;g++)a[e+g]=d[g]}else a.push(d)}}
;function mb(a){var b=!1,c;return function(){b||(c=a(),b=!0);return c}}
;function nb(a,b){for(var c in a)b.call(void 0,a[c],c,a)}
function ob(a){var b=pb,c;for(c in b)if(a.call(void 0,b[c],c,b))return c}
function qb(a,b){for(var c in a)if(!(c in b)||a[c]!==b[c])return!1;for(var d in b)if(!(d in a))return!1;return!0}
function rb(a){var b=Pa(a);if("object"==b||"array"==b){if(Sa(a.clone))return a.clone();b="array"==b?[]:{};for(var c in a)b[c]=rb(a[c]);return b}return a}
var sb="constructor hasOwnProperty isPrototypeOf propertyIsEnumerable toLocaleString toString valueOf".split(" ");function tb(a,b){for(var c,d,e=1;e<arguments.length;e++){d=arguments[e];for(c in d)a[c]=d[c];for(var f=0;f<sb.length;f++)c=sb[f],Object.prototype.hasOwnProperty.call(d,c)&&(a[c]=d[c])}}
;var ub;var vb=String.prototype.trim?function(a){return a.trim()}:function(a){return/^[\s\xa0]*([\s\S]*?)[\s\xa0]*$/.exec(a)[1]},wb=/&/g,xb=/</g,yb=/>/g,zb=/"/g,Ab=/'/g,Bb=/\x00/g,Cb=/[\x00&<>"']/;var I;a:{var Db=A.navigator;if(Db){var Eb=Db.userAgent;if(Eb){I=Eb;break a}}I=""}function J(a){return-1!=I.indexOf(a)}
;function Fb(){}
;var Gb=J("Opera"),Hb=J("Trident")||J("MSIE"),Ib=J("Edge"),Jb=J("Gecko")&&!(-1!=I.toLowerCase().indexOf("webkit")&&!J("Edge"))&&!(J("Trident")||J("MSIE"))&&!J("Edge"),Kb=-1!=I.toLowerCase().indexOf("webkit")&&!J("Edge");function Lb(){var a=A.document;return a?a.documentMode:void 0}
var Ub;a:{var Vb="",Wb=function(){var a=I;if(Jb)return/rv:([^\);]+)(\)|;)/.exec(a);if(Ib)return/Edge\/([\d\.]+)/.exec(a);if(Hb)return/\b(?:MSIE|rv)[: ]([^\);]+)(\)|;)/.exec(a);if(Kb)return/WebKit\/(\S+)/.exec(a);if(Gb)return/(?:Version)[ \/]?(\S+)/.exec(a)}();
Wb&&(Vb=Wb?Wb[1]:"");if(Hb){var Xb=Lb();if(null!=Xb&&Xb>parseFloat(Vb)){Ub=String(Xb);break a}}Ub=Vb}var Yb=Ub,Zb;if(A.document&&Hb){var $b=Lb();Zb=$b?$b:parseInt(Yb,10)||void 0}else Zb=void 0;var ac=Zb;var bc=J("iPhone")&&!J("iPod")&&!J("iPad")||J("iPod"),cc=J("iPad");var dc={},ec=null;var K=window;function fc(a){var b=B("window.location.href");null==a&&(a='Unknown Error of type "null/undefined"');if("string"===typeof a)return{message:a,name:"Unknown error",lineNumber:"Not available",fileName:b,stack:"Not available"};var c=!1;try{var d=a.lineNumber||a.line||"Not available"}catch(g){d="Not available",c=!0}try{var e=a.fileName||a.filename||a.sourceURL||A.$googDebugFname||b}catch(g){e="Not available",c=!0}b=gc(a);if(!(!c&&a.lineNumber&&a.fileName&&a.stack&&a.message&&a.name)){c=a.message;if(null==
c){if(a.constructor&&a.constructor instanceof Function){if(a.constructor.name)c=a.constructor.name;else if(c=a.constructor,hc[c])c=hc[c];else{c=String(c);if(!hc[c]){var f=/function\s+([^\(]+)/m.exec(c);hc[c]=f?f[1]:"[Anonymous]"}c=hc[c]}c='Unknown Error of type "'+c+'"'}else c="Unknown Error of unknown type";"function"===typeof a.toString&&Object.prototype.toString!==a.toString&&(c+=": "+a.toString())}return{message:c,name:a.name||"UnknownError",lineNumber:d,fileName:e,stack:b||"Not available"}}a.stack=
b;return a}
function gc(a,b){b||(b={});b[ic(a)]=!0;var c=a.stack||"",d=a.Da;d&&!b[ic(d)]&&(c+="\nCaused by: ",d.stack&&0==d.stack.indexOf(d.toString())||(c+="string"===typeof d?d:d.message+"\n"),c+=gc(d,b));return c}
function ic(a){var b="";"function"===typeof a.toString&&(b=""+a);return b+a.stack}
var hc={};function jc(a){this.f=a||{cookie:""}}
p=jc.prototype;p.isEnabled=function(){return navigator.cookieEnabled};
p.set=function(a,b,c){var d=!1;if("object"===typeof c){var e=c.Ka;d=c.secure||!1;var f=c.domain||void 0;var g=c.path||void 0;var h=c.V}if(/[;=\s]/.test(a))throw Error('Invalid cookie name "'+a+'"');if(/[;\r\n]/.test(b))throw Error('Invalid cookie value "'+b+'"');void 0===h&&(h=-1);c=f?";domain="+f:"";g=g?";path="+g:"";d=d?";secure":"";h=0>h?"":0==h?";expires="+(new Date(1970,1,1)).toUTCString():";expires="+(new Date(C()+1E3*h)).toUTCString();this.f.cookie=a+"="+b+c+g+h+d+(null!=e?";samesite="+e:"")};
p.get=function(a,b){for(var c=a+"=",d=(this.f.cookie||"").split(";"),e=0,f;e<d.length;e++){f=vb(d[e]);if(0==f.lastIndexOf(c,0))return f.substr(c.length);if(f==a)return""}return b};
p.remove=function(a,b,c){var d=void 0!==this.get(a);this.set(a,"",{V:0,path:b,domain:c});return d};
p.isEmpty=function(){return!this.f.cookie};
p.clear=function(){for(var a=(this.f.cookie||"").split(";"),b=[],c=[],d,e,f=0;f<a.length;f++)e=vb(a[f]),d=e.indexOf("="),-1==d?(b.push(""),c.push(e)):(b.push(e.substring(0,d)),c.push(e.substring(d+1)));for(a=b.length-1;0<=a;a--)this.remove(b[a])};
var kc=new jc("undefined"==typeof document?null:document);function lc(a,b){this.width=a;this.height=b}
p=lc.prototype;p.clone=function(){return new lc(this.width,this.height)};
p.aspectRatio=function(){return this.width/this.height};
p.isEmpty=function(){return!(this.width*this.height)};
p.ceil=function(){this.width=Math.ceil(this.width);this.height=Math.ceil(this.height);return this};
p.floor=function(){this.width=Math.floor(this.width);this.height=Math.floor(this.height);return this};
p.round=function(){this.width=Math.round(this.width);this.height=Math.round(this.height);return this};function mc(a,b){var c,d;var e=document;e=b||e;if(e.querySelectorAll&&e.querySelector&&a)return e.querySelectorAll(a?"."+a:"");if(a&&e.getElementsByClassName){var f=e.getElementsByClassName(a);return f}f=e.getElementsByTagName("*");if(a){var g={};for(c=d=0;e=f[c];c++){var h=e.className,k;if(k="function"==typeof h.split)k=0<=bb(h.split(/\s+/),a);k&&(g[d++]=e)}g.length=d;return g}return f}
function nc(){var a=document;var b="IFRAME";"application/xhtml+xml"===a.contentType&&(b=b.toLowerCase());return a.createElement(b)}
function oc(a,b){for(var c=0;a;){if(b(a))return a;a=a.parentNode;c++}return null}
;var pc=/^(?:([^:/?#.]+):)?(?:\/\/(?:([^\\/?#]*)@)?([^\\/?#]*?)(?::([0-9]+))?(?=[\\/?#]|$))?([^?#]+)?(?:\?([^#]*))?(?:#([\s\S]*))?$/;function qc(a){return a?decodeURI(a):a}
function L(a){return qc(a.match(pc)[3]||null)}
function rc(a){var b=a.match(pc);a=b[1];var c=b[2],d=b[3];b=b[4];var e="";a&&(e+=a+":");d&&(e+="//",c&&(e+=c+"@"),e+=d,b&&(e+=":"+b));return e}
function sc(a,b,c){if(Array.isArray(b))for(var d=0;d<b.length;d++)sc(a,String(b[d]),c);else null!=b&&c.push(a+(""===b?"":"="+encodeURIComponent(String(b))))}
function tc(a){var b=[],c;for(c in a)sc(c,a[c],b);return b.join("&")}
var uc=/#|$/;function vc(a){var b=wc;if(b)for(var c in b)Object.prototype.hasOwnProperty.call(b,c)&&a.call(void 0,b[c],c,b)}
function xc(){var a=[];vc(function(b){a.push(b)});
return a}
var wc={qa:"allow-forms",ra:"allow-modals",sa:"allow-orientation-lock",ta:"allow-pointer-lock",ua:"allow-popups",va:"allow-popups-to-escape-sandbox",wa:"allow-presentation",xa:"allow-same-origin",ya:"allow-scripts",za:"allow-top-navigation",Aa:"allow-top-navigation-by-user-activation"},yc=mb(function(){return xc()});
function zc(){var a=nc(),b={};H(yc(),function(c){a.sandbox&&a.sandbox.supports&&a.sandbox.supports(c)&&(b[c]=!0)});
return b}
;function Ac(){this.h=this.h;this.i=this.i}
Ac.prototype.h=!1;Ac.prototype.dispose=function(){this.h||(this.h=!0,this.O())};
Ac.prototype.O=function(){if(this.i)for(;this.i.length;)this.i.shift()()};var Bc={};function Cc(){}
function Dc(a,b){if(b!==Bc)throw Error("Bad secret");this.f=a}
sa(Dc,Cc);Dc.prototype.toString=function(){return this.f};new Dc("about:blank",Bc);new Dc("about:invalid#zTSz",Bc);var Ec=(new Date).getTime();function Fc(a){if(!a)return"";a=a.split("#")[0].split("?")[0];a=a.toLowerCase();0==a.indexOf("//")&&(a=window.location.protocol+a);/^[\w\-]*:\/\//.test(a)||(a=window.location.href);var b=a.substring(a.indexOf("://")+3),c=b.indexOf("/");-1!=c&&(b=b.substring(0,c));a=a.substring(0,a.indexOf("://"));if("http"!==a&&"https"!==a&&"chrome-extension"!==a&&"moz-extension"!==a&&"file"!==a&&"android-app"!==a&&"chrome-search"!==a&&"chrome-untrusted"!==a&&"chrome"!==a&&"app"!==a&&"devtools"!==a)throw Error("Invalid URI scheme in origin: "+
a);c="";var d=b.indexOf(":");if(-1!=d){var e=b.substring(d+1);b=b.substring(0,d);if("http"===a&&"80"!==e||"https"===a&&"443"!==e)c=":"+e}return a+"://"+b+c}
;function Gc(){function a(){e[0]=1732584193;e[1]=4023233417;e[2]=2562383102;e[3]=271733878;e[4]=3285377520;m=l=0}
function b(n){for(var r=g,q=0;64>q;q+=4)r[q/4]=n[q]<<24|n[q+1]<<16|n[q+2]<<8|n[q+3];for(q=16;80>q;q++)n=r[q-3]^r[q-8]^r[q-14]^r[q-16],r[q]=(n<<1|n>>>31)&4294967295;n=e[0];var v=e[1],w=e[2],G=e[3],Ra=e[4];for(q=0;80>q;q++){if(40>q)if(20>q){var ka=G^v&(w^G);var Da=1518500249}else ka=v^w^G,Da=1859775393;else 60>q?(ka=v&w|G&(v|w),Da=2400959708):(ka=v^w^G,Da=3395469782);ka=((n<<5|n>>>27)&4294967295)+ka+Ra+Da+r[q]&4294967295;Ra=G;G=w;w=(v<<30|v>>>2)&4294967295;v=n;n=ka}e[0]=e[0]+n&4294967295;e[1]=e[1]+
v&4294967295;e[2]=e[2]+w&4294967295;e[3]=e[3]+G&4294967295;e[4]=e[4]+Ra&4294967295}
function c(n,r){if("string"===typeof n){n=unescape(encodeURIComponent(n));for(var q=[],v=0,w=n.length;v<w;++v)q.push(n.charCodeAt(v));n=q}r||(r=n.length);q=0;if(0==l)for(;q+64<r;)b(n.slice(q,q+64)),q+=64,m+=64;for(;q<r;)if(f[l++]=n[q++],m++,64==l)for(l=0,b(f);q+64<r;)b(n.slice(q,q+64)),q+=64,m+=64}
function d(){var n=[],r=8*m;56>l?c(h,56-l):c(h,64-(l-56));for(var q=63;56<=q;q--)f[q]=r&255,r>>>=8;b(f);for(q=r=0;5>q;q++)for(var v=24;0<=v;v-=8)n[r++]=e[q]>>v&255;return n}
for(var e=[],f=[],g=[],h=[128],k=1;64>k;++k)h[k]=0;var l,m;a();return{reset:a,update:c,digest:d,ea:function(){for(var n=d(),r="",q=0;q<n.length;q++)r+="0123456789ABCDEF".charAt(Math.floor(n[q]/16))+"0123456789ABCDEF".charAt(n[q]%16);return r}}}
;function Hc(a,b,c){var d=[],e=[];if(1==(Array.isArray(c)?2:1))return e=[b,a],H(d,function(h){e.push(h)}),Ic(e.join(" "));
var f=[],g=[];H(c,function(h){g.push(h.key);f.push(h.value)});
c=Math.floor((new Date).getTime()/1E3);e=0==f.length?[c,b,a]:[f.join(":"),c,b,a];H(d,function(h){e.push(h)});
a=Ic(e.join(" "));a=[c,a];0==g.length||a.push(g.join(""));return a.join("_")}
function Ic(a){var b=Gc();b.update(a);return b.ea().toLowerCase()}
;function Jc(a){var b=Fc(String(A.location.href)),c;(c=A.__SAPISID||A.__APISID||A.__OVERRIDE_SID)?c=!0:(c=new jc(document),c=c.get("SAPISID")||c.get("APISID")||c.get("__Secure-3PAPISID")||c.get("SID"),c=!!c);if(c&&(c=(b=0==b.indexOf("https:")||0==b.indexOf("chrome-extension:")||0==b.indexOf("moz-extension:"))?A.__SAPISID:A.__APISID,c||(c=new jc(document),c=c.get(b?"SAPISID":"APISID")||c.get("__Secure-3PAPISID")),c)){b=b?"SAPISIDHASH":"APISIDHASH";var d=String(A.location.href);return d&&c&&b?[b,Hc(Fc(d),
c,a||null)].join(" "):null}return null}
;function Kc(){this.g=[];this.f=-1}
Kc.prototype.set=function(a,b){b=void 0===b?!0:b;0<=a&&52>a&&0===a%1&&this.g[a]!=b&&(this.g[a]=b,this.f=-1)};
Kc.prototype.get=function(a){return!!this.g[a]};
function Lc(a){-1==a.f&&(a.f=hb(a.g,function(b,c,d){return c?b+Math.pow(2,d):b},0));
return a.f}
;function Mc(a,b){this.h=a;this.i=b;this.g=0;this.f=null}
Mc.prototype.get=function(){if(0<this.g){this.g--;var a=this.f;this.f=a.next;a.next=null}else a=this.h();return a};
function Nc(a,b){a.i(b);100>a.g&&(a.g++,b.next=a.f,a.f=b)}
;function Oc(a){A.setTimeout(function(){throw a;},0)}
var Pc;function Qc(){var a=A.MessageChannel;"undefined"===typeof a&&"undefined"!==typeof window&&window.postMessage&&window.addEventListener&&!J("Presto")&&(a=function(){var e=nc();e.style.display="none";document.documentElement.appendChild(e);var f=e.contentWindow;e=f.document;e.open();e.close();var g="callImmediate"+Math.random(),h="file:"==f.location.protocol?"*":f.location.protocol+"//"+f.location.host;e=Za(function(k){if(("*"==h||k.origin==h)&&k.data==g)this.port1.onmessage()},this);
f.addEventListener("message",e,!1);this.port1={};this.port2={postMessage:function(){f.postMessage(g,h)}}});
if("undefined"!==typeof a&&!J("Trident")&&!J("MSIE")){var b=new a,c={},d=c;b.port1.onmessage=function(){if(void 0!==c.next){c=c.next;var e=c.T;c.T=null;e()}};
return function(e){d.next={T:e};d=d.next;b.port2.postMessage(0)}}return function(e){A.setTimeout(e,0)}}
;function Rc(){this.g=this.f=null}
var Tc=new Mc(function(){return new Sc},function(a){a.reset()});
Rc.prototype.add=function(a,b){var c=Tc.get();c.set(a,b);this.g?this.g.next=c:this.f=c;this.g=c};
Rc.prototype.remove=function(){var a=null;this.f&&(a=this.f,this.f=this.f.next,this.f||(this.g=null),a.next=null);return a};
function Sc(){this.next=this.scope=this.f=null}
Sc.prototype.set=function(a,b){this.f=a;this.scope=b;this.next=null};
Sc.prototype.reset=function(){this.next=this.scope=this.f=null};function Uc(a,b){Vc||Wc();Xc||(Vc(),Xc=!0);Yc.add(a,b)}
var Vc;function Wc(){if(A.Promise&&A.Promise.resolve){var a=A.Promise.resolve(void 0);Vc=function(){a.then(Zc)}}else Vc=function(){var b=Zc;
!Sa(A.setImmediate)||A.Window&&A.Window.prototype&&!J("Edge")&&A.Window.prototype.setImmediate==A.setImmediate?(Pc||(Pc=Qc()),Pc(b)):A.setImmediate(b)}}
var Xc=!1,Yc=new Rc;function Zc(){for(var a;a=Yc.remove();){try{a.f.call(a.scope)}catch(b){Oc(b)}Nc(Tc,a)}Xc=!1}
;function $c(){this.g=-1}
;function ud(){this.g=64;this.f=[];this.l=[];this.m=[];this.i=[];this.i[0]=128;for(var a=1;a<this.g;++a)this.i[a]=0;this.j=this.h=0;this.reset()}
E(ud,$c);ud.prototype.reset=function(){this.f[0]=1732584193;this.f[1]=4023233417;this.f[2]=2562383102;this.f[3]=271733878;this.f[4]=3285377520;this.j=this.h=0};
function vd(a,b,c){c||(c=0);var d=a.m;if("string"===typeof b)for(var e=0;16>e;e++)d[e]=b.charCodeAt(c)<<24|b.charCodeAt(c+1)<<16|b.charCodeAt(c+2)<<8|b.charCodeAt(c+3),c+=4;else for(e=0;16>e;e++)d[e]=b[c]<<24|b[c+1]<<16|b[c+2]<<8|b[c+3],c+=4;for(e=16;80>e;e++){var f=d[e-3]^d[e-8]^d[e-14]^d[e-16];d[e]=(f<<1|f>>>31)&4294967295}b=a.f[0];c=a.f[1];var g=a.f[2],h=a.f[3],k=a.f[4];for(e=0;80>e;e++){if(40>e)if(20>e){f=h^c&(g^h);var l=1518500249}else f=c^g^h,l=1859775393;else 60>e?(f=c&g|h&(c|g),l=2400959708):
(f=c^g^h,l=3395469782);f=(b<<5|b>>>27)+f+k+l+d[e]&4294967295;k=h;h=g;g=(c<<30|c>>>2)&4294967295;c=b;b=f}a.f[0]=a.f[0]+b&4294967295;a.f[1]=a.f[1]+c&4294967295;a.f[2]=a.f[2]+g&4294967295;a.f[3]=a.f[3]+h&4294967295;a.f[4]=a.f[4]+k&4294967295}
ud.prototype.update=function(a,b){if(null!=a){void 0===b&&(b=a.length);for(var c=b-this.g,d=0,e=this.l,f=this.h;d<b;){if(0==f)for(;d<=c;)vd(this,a,d),d+=this.g;if("string"===typeof a)for(;d<b;){if(e[f]=a.charCodeAt(d),++f,++d,f==this.g){vd(this,e);f=0;break}}else for(;d<b;)if(e[f]=a[d],++f,++d,f==this.g){vd(this,e);f=0;break}}this.h=f;this.j+=b}};
ud.prototype.digest=function(){var a=[],b=8*this.j;56>this.h?this.update(this.i,56-this.h):this.update(this.i,this.g-(this.h-56));for(var c=this.g-1;56<=c;c--)this.l[c]=b&255,b/=256;vd(this,this.l);for(c=b=0;5>c;c++)for(var d=24;0<=d;d-=8)a[b]=this.f[c]>>d&255,++b;return a};var wd="StopIteration"in A?A.StopIteration:{message:"StopIteration",stack:""};function xd(){}
xd.prototype.next=function(){throw wd;};
xd.prototype.A=function(){return this};
function yd(a){if(a instanceof xd)return a;if("function"==typeof a.A)return a.A(!1);if(Qa(a)){var b=0,c=new xd;c.next=function(){for(;;){if(b>=a.length)throw wd;if(b in a)return a[b++];b++}};
return c}throw Error("Not implemented");}
function zd(a,b){if(Qa(a))try{H(a,b,void 0)}catch(c){if(c!==wd)throw c;}else{a=yd(a);try{for(;;)b.call(void 0,a.next(),void 0,a)}catch(c){if(c!==wd)throw c;}}}
function Ad(a){if(Qa(a))return kb(a);a=yd(a);var b=[];zd(a,function(c){b.push(c)});
return b}
;function Bd(a,b){this.h={};this.f=[];this.i=this.g=0;var c=arguments.length;if(1<c){if(c%2)throw Error("Uneven number of arguments");for(var d=0;d<c;d+=2)this.set(arguments[d],arguments[d+1])}else if(a)if(a instanceof Bd)for(c=Cd(a),d=0;d<c.length;d++)this.set(c[d],a.get(c[d]));else for(d in a)this.set(d,a[d])}
function Cd(a){Dd(a);return a.f.concat()}
p=Bd.prototype;p.equals=function(a,b){if(this===a)return!0;if(this.g!=a.g)return!1;var c=b||Ed;Dd(this);for(var d,e=0;d=this.f[e];e++)if(!c(this.get(d),a.get(d)))return!1;return!0};
function Ed(a,b){return a===b}
p.isEmpty=function(){return 0==this.g};
p.clear=function(){this.h={};this.i=this.g=this.f.length=0};
p.remove=function(a){return Object.prototype.hasOwnProperty.call(this.h,a)?(delete this.h[a],this.g--,this.i++,this.f.length>2*this.g&&Dd(this),!0):!1};
function Dd(a){if(a.g!=a.f.length){for(var b=0,c=0;b<a.f.length;){var d=a.f[b];Object.prototype.hasOwnProperty.call(a.h,d)&&(a.f[c++]=d);b++}a.f.length=c}if(a.g!=a.f.length){var e={};for(c=b=0;b<a.f.length;)d=a.f[b],Object.prototype.hasOwnProperty.call(e,d)||(a.f[c++]=d,e[d]=1),b++;a.f.length=c}}
p.get=function(a,b){return Object.prototype.hasOwnProperty.call(this.h,a)?this.h[a]:b};
p.set=function(a,b){Object.prototype.hasOwnProperty.call(this.h,a)||(this.g++,this.f.push(a),this.i++);this.h[a]=b};
p.forEach=function(a,b){for(var c=Cd(this),d=0;d<c.length;d++){var e=c[d],f=this.get(e);a.call(b,f,e,this)}};
p.clone=function(){return new Bd(this)};
p.A=function(a){Dd(this);var b=0,c=this.i,d=this,e=new xd;e.next=function(){if(c!=d.i)throw Error("The map has changed since the iterator was created");if(b>=d.f.length)throw wd;var f=d.f[b++];return a?f:d.h[f]};
return e};var Fd=A.JSON.stringify;function M(a){this.f=0;this.m=void 0;this.i=this.g=this.h=null;this.j=this.l=!1;if(a!=Oa)try{var b=this;a.call(void 0,function(c){Gd(b,2,c)},function(c){Gd(b,3,c)})}catch(c){Gd(this,3,c)}}
function Hd(){this.next=this.context=this.onRejected=this.g=this.f=null;this.h=!1}
Hd.prototype.reset=function(){this.context=this.onRejected=this.g=this.f=null;this.h=!1};
var Id=new Mc(function(){return new Hd},function(a){a.reset()});
function Jd(a,b,c){var d=Id.get();d.g=a;d.onRejected=b;d.context=c;return d}
function Kd(a){if(a instanceof M)return a;var b=new M(Oa);Gd(b,2,a);return b}
function Ld(a){return new M(function(b,c){c(a)})}
M.prototype.then=function(a,b,c){return Md(this,Sa(a)?a:null,Sa(b)?b:null,c)};
M.prototype.$goog_Thenable=!0;function Nd(a,b){return Md(a,null,b,void 0)}
M.prototype.cancel=function(a){if(0==this.f){var b=new Od(a);Uc(function(){Pd(this,b)},this)}};
function Pd(a,b){if(0==a.f)if(a.h){var c=a.h;if(c.g){for(var d=0,e=null,f=null,g=c.g;g&&(g.h||(d++,g.f==a&&(e=g),!(e&&1<d)));g=g.next)e||(f=g);e&&(0==c.f&&1==d?Pd(c,b):(f?(d=f,d.next==c.i&&(c.i=d),d.next=d.next.next):Qd(c),Rd(c,e,3,b)))}a.h=null}else Gd(a,3,b)}
function Sd(a,b){a.g||2!=a.f&&3!=a.f||Td(a);a.i?a.i.next=b:a.g=b;a.i=b}
function Md(a,b,c,d){var e=Jd(null,null,null);e.f=new M(function(f,g){e.g=b?function(h){try{var k=b.call(d,h);f(k)}catch(l){g(l)}}:f;
e.onRejected=c?function(h){try{var k=c.call(d,h);void 0===k&&h instanceof Od?g(h):f(k)}catch(l){g(l)}}:g});
e.f.h=a;Sd(a,e);return e.f}
M.prototype.v=function(a){this.f=0;Gd(this,2,a)};
M.prototype.G=function(a){this.f=0;Gd(this,3,a)};
function Gd(a,b,c){if(0==a.f){a===c&&(b=3,c=new TypeError("Promise cannot resolve to itself"));a.f=1;a:{var d=c,e=a.v,f=a.G;if(d instanceof M){Sd(d,Jd(e||Oa,f||null,a));var g=!0}else{if(d)try{var h=!!d.$goog_Thenable}catch(l){h=!1}else h=!1;if(h)d.then(e,f,a),g=!0;else{if(Ta(d))try{var k=d.then;if(Sa(k)){Ud(d,k,e,f,a);g=!0;break a}}catch(l){f.call(a,l);g=!0;break a}g=!1}}}g||(a.m=c,a.f=b,a.h=null,Td(a),3!=b||c instanceof Od||Vd(a,c))}}
function Ud(a,b,c,d,e){function f(k){h||(h=!0,d.call(e,k))}
function g(k){h||(h=!0,c.call(e,k))}
var h=!1;try{b.call(a,g,f)}catch(k){f(k)}}
function Td(a){a.l||(a.l=!0,Uc(a.s,a))}
function Qd(a){var b=null;a.g&&(b=a.g,a.g=b.next,b.next=null);a.g||(a.i=null);return b}
M.prototype.s=function(){for(var a;a=Qd(this);)Rd(this,a,this.f,this.m);this.l=!1};
function Rd(a,b,c,d){if(3==c&&b.onRejected&&!b.h)for(;a&&a.j;a=a.h)a.j=!1;if(b.f)b.f.h=null,Wd(b,c,d);else try{b.h?b.g.call(b.context):Wd(b,c,d)}catch(e){Xd.call(null,e)}Nc(Id,b)}
function Wd(a,b,c){2==b?a.g.call(a.context,c):a.onRejected&&a.onRejected.call(a.context,c)}
function Vd(a,b){a.j=!0;Uc(function(){a.j&&Xd.call(null,b)})}
var Xd=Oc;function Od(a){ab.call(this,a)}
E(Od,ab);Od.prototype.name="cancel";function O(a){Ac.call(this);this.m=1;this.j=[];this.l=0;this.f=[];this.g={};this.s=!!a}
E(O,Ac);p=O.prototype;p.subscribe=function(a,b,c){var d=this.g[a];d||(d=this.g[a]=[]);var e=this.m;this.f[e]=a;this.f[e+1]=b;this.f[e+2]=c;this.m=e+3;d.push(e);return e};
function Yd(a,b,c){var d=Zd;if(a=d.g[a]){var e=d.f;(a=ib(a,function(f){return e[f+1]==b&&e[f+2]==c}))&&d.L(a)}}
p.L=function(a){var b=this.f[a];if(b){var c=this.g[b];if(0!=this.l)this.j.push(a),this.f[a+1]=Oa;else{if(c){var d=bb(c,a);0<=d&&Array.prototype.splice.call(c,d,1)}delete this.f[a];delete this.f[a+1];delete this.f[a+2]}}return!!b};
p.J=function(a,b){var c=this.g[a];if(c){for(var d=Array(arguments.length-1),e=1,f=arguments.length;e<f;e++)d[e-1]=arguments[e];if(this.s)for(e=0;e<c.length;e++){var g=c[e];$d(this.f[g+1],this.f[g+2],d)}else{this.l++;try{for(e=0,f=c.length;e<f;e++)g=c[e],this.f[g+1].apply(this.f[g+2],d)}finally{if(this.l--,0<this.j.length&&0==this.l)for(;c=this.j.pop();)this.L(c)}}return 0!=e}return!1};
function $d(a,b,c){Uc(function(){a.apply(b,c)})}
p.clear=function(a){if(a){var b=this.g[a];b&&(H(b,this.L,this),delete this.g[a])}else this.f.length=0,this.g={}};
p.O=function(){O.F.O.call(this);this.clear();this.j.length=0};function ae(a){this.f=a}
ae.prototype.set=function(a,b){void 0===b?this.f.remove(a):this.f.set(a,Fd(b))};
ae.prototype.get=function(a){try{var b=this.f.get(a)}catch(c){return}if(null!==b)try{return JSON.parse(b)}catch(c){throw"Storage: Invalid value was encountered";}};
ae.prototype.remove=function(a){this.f.remove(a)};function be(a){this.f=a}
E(be,ae);function ce(a){this.data=a}
function de(a){return void 0===a||a instanceof ce?a:new ce(a)}
be.prototype.set=function(a,b){be.F.set.call(this,a,de(b))};
be.prototype.g=function(a){a=be.F.get.call(this,a);if(void 0===a||a instanceof Object)return a;throw"Storage: Invalid value was encountered";};
be.prototype.get=function(a){if(a=this.g(a)){if(a=a.data,void 0===a)throw"Storage: Invalid value was encountered";}else a=void 0;return a};function ee(a){this.f=a}
E(ee,be);ee.prototype.set=function(a,b,c){if(b=de(b)){if(c){if(c<C()){ee.prototype.remove.call(this,a);return}b.expiration=c}b.creation=C()}ee.F.set.call(this,a,b)};
ee.prototype.g=function(a){var b=ee.F.g.call(this,a);if(b){var c=b.creation,d=b.expiration;if(d&&d<C()||c&&c>C())ee.prototype.remove.call(this,a);else return b}};function fe(){}
;function ge(){}
E(ge,fe);ge.prototype.clear=function(){var a=Ad(this.A(!0)),b=this;H(a,function(c){b.remove(c)})};function he(a){this.f=a}
E(he,ge);p=he.prototype;p.isAvailable=function(){if(!this.f)return!1;try{return this.f.setItem("__sak","1"),this.f.removeItem("__sak"),!0}catch(a){return!1}};
p.set=function(a,b){try{this.f.setItem(a,b)}catch(c){if(0==this.f.length)throw"Storage mechanism: Storage disabled";throw"Storage mechanism: Quota exceeded";}};
p.get=function(a){a=this.f.getItem(a);if("string"!==typeof a&&null!==a)throw"Storage mechanism: Invalid value was encountered";return a};
p.remove=function(a){this.f.removeItem(a)};
p.A=function(a){var b=0,c=this.f,d=new xd;d.next=function(){if(b>=c.length)throw wd;var e=c.key(b++);if(a)return e;e=c.getItem(e);if("string"!==typeof e)throw"Storage mechanism: Invalid value was encountered";return e};
return d};
p.clear=function(){this.f.clear()};
p.key=function(a){return this.f.key(a)};function ie(){var a=null;try{a=window.localStorage||null}catch(b){}this.f=a}
E(ie,he);function je(a,b){this.g=a;this.f=null;if(Hb&&!(9<=Number(ac))){ke||(ke=new Bd);this.f=ke.get(a);this.f||(b?this.f=document.getElementById(b):(this.f=document.createElement("userdata"),this.f.addBehavior("#default#userData"),document.body.appendChild(this.f)),ke.set(a,this.f));try{this.f.load(this.g)}catch(c){this.f=null}}}
E(je,ge);var le={".":".2E","!":".21","~":".7E","*":".2A","'":".27","(":".28",")":".29","%":"."},ke=null;function me(a){return"_"+encodeURIComponent(a).replace(/[.!~*'()%]/g,function(b){return le[b]})}
p=je.prototype;p.isAvailable=function(){return!!this.f};
p.set=function(a,b){this.f.setAttribute(me(a),b);ne(this)};
p.get=function(a){a=this.f.getAttribute(me(a));if("string"!==typeof a&&null!==a)throw"Storage mechanism: Invalid value was encountered";return a};
p.remove=function(a){this.f.removeAttribute(me(a));ne(this)};
p.A=function(a){var b=0,c=this.f.XMLDocument.documentElement.attributes,d=new xd;d.next=function(){if(b>=c.length)throw wd;var e=c[b++];if(a)return decodeURIComponent(e.nodeName.replace(/\./g,"%")).substr(1);e=e.nodeValue;if("string"!==typeof e)throw"Storage mechanism: Invalid value was encountered";return e};
return d};
p.clear=function(){for(var a=this.f.XMLDocument.documentElement,b=a.attributes.length;0<b;b--)a.removeAttribute(a.attributes[b-1].nodeName);ne(this)};
function ne(a){try{a.f.save(a.g)}catch(b){throw"Storage mechanism: Quota exceeded";}}
;function oe(a,b){this.g=a;this.f=b+"::"}
E(oe,ge);oe.prototype.set=function(a,b){this.g.set(this.f+a,b)};
oe.prototype.get=function(a){return this.g.get(this.f+a)};
oe.prototype.remove=function(a){this.g.remove(this.f+a)};
oe.prototype.A=function(a){var b=this.g.A(!0),c=this,d=new xd;d.next=function(){for(var e=b.next();e.substr(0,c.f.length)!=c.f;)e=b.next();return a?e.substr(c.f.length):c.g.get(e)};
return d};var pe=window.yt&&window.yt.config_||window.ytcfg&&window.ytcfg.data_||{};D("yt.config_",pe);function qe(a){var b=arguments;1<b.length?pe[b[0]]=b[1]:1===b.length&&Object.assign(pe,b[0])}
function P(a,b){return a in pe?pe[a]:b}
;var re=[];function se(a){re.forEach(function(b){return b(a)})}
function te(a){return a&&window.yterr?function(){try{return a.apply(this,arguments)}catch(b){ue(b),se(b)}}:a}
function ue(a){var b=B("yt.logging.errors.log");b?b(a,"ERROR",void 0,void 0,void 0):(b=P("ERRORS",[]),b.push([a,"ERROR",void 0,void 0,void 0]),qe("ERRORS",b))}
function ve(a){var b=B("yt.logging.errors.log");b?b(a,"WARNING",void 0,void 0,void 0):(b=P("ERRORS",[]),b.push([a,"WARNING",void 0,void 0,void 0]),qe("ERRORS",b))}
;var we=0;D("ytDomDomGetNextId",B("ytDomDomGetNextId")||function(){return++we});var xe={stopImmediatePropagation:1,stopPropagation:1,preventMouseEvent:1,preventManipulation:1,preventDefault:1,layerX:1,layerY:1,screenX:1,screenY:1,scale:1,rotation:1,webkitMovementX:1,webkitMovementY:1};
function ye(a){this.type="";this.state=this.source=this.data=this.currentTarget=this.relatedTarget=this.target=null;this.charCode=this.keyCode=0;this.metaKey=this.shiftKey=this.ctrlKey=this.altKey=!1;this.clientY=this.clientX=0;this.changedTouches=this.touches=null;try{if(a=a||window.event){this.event=a;for(var b in a)b in xe||(this[b]=a[b]);var c=a.target||a.srcElement;c&&3==c.nodeType&&(c=c.parentNode);this.target=c;var d=a.relatedTarget;if(d)try{d=d.nodeName?d:null}catch(e){d=null}else"mouseover"==
this.type?d=a.fromElement:"mouseout"==this.type&&(d=a.toElement);this.relatedTarget=d;this.clientX=void 0!=a.clientX?a.clientX:a.pageX;this.clientY=void 0!=a.clientY?a.clientY:a.pageY;this.keyCode=a.keyCode?a.keyCode:a.which;this.charCode=a.charCode||("keypress"==this.type?this.keyCode:0);this.altKey=a.altKey;this.ctrlKey=a.ctrlKey;this.shiftKey=a.shiftKey;this.metaKey=a.metaKey}}catch(e){}}
ye.prototype.preventDefault=function(){this.event&&(this.event.returnValue=!1,this.event.preventDefault&&this.event.preventDefault())};
ye.prototype.stopPropagation=function(){this.event&&(this.event.cancelBubble=!0,this.event.stopPropagation&&this.event.stopPropagation())};
ye.prototype.stopImmediatePropagation=function(){this.event&&(this.event.cancelBubble=!0,this.event.stopImmediatePropagation&&this.event.stopImmediatePropagation())};var pb=A.ytEventsEventsListeners||{};D("ytEventsEventsListeners",pb);var ze=A.ytEventsEventsCounter||{count:0};D("ytEventsEventsCounter",ze);
function Ae(a,b,c,d){d=void 0===d?{}:d;a.addEventListener&&("mouseenter"!=b||"onmouseenter"in document?"mouseleave"!=b||"onmouseenter"in document?"mousewheel"==b&&"MozBoxSizing"in document.documentElement.style&&(b="MozMousePixelScroll"):b="mouseout":b="mouseover");return ob(function(e){var f="boolean"===typeof e[4]&&e[4]==!!d,g=Ta(e[4])&&Ta(d)&&qb(e[4],d);return!!e.length&&e[0]==a&&e[1]==b&&e[2]==c&&(f||g)})}
function Be(a){a&&("string"==typeof a&&(a=[a]),H(a,function(b){if(b in pb){var c=pb[b],d=c[0],e=c[1],f=c[3];c=c[4];d.removeEventListener?Ce()||"boolean"===typeof c?d.removeEventListener(e,f,c):d.removeEventListener(e,f,!!c.capture):d.detachEvent&&d.detachEvent("on"+e,f);delete pb[b]}}))}
var Ce=mb(function(){var a=!1;try{var b=Object.defineProperty({},"capture",{get:function(){a=!0}});
window.addEventListener("test",null,b)}catch(c){}return a});
function De(a,b,c){var d=void 0===d?{}:d;if(a&&(a.addEventListener||a.attachEvent)){var e=Ae(a,b,c,d);if(!e){e=++ze.count+"";var f=!("mouseenter"!=b&&"mouseleave"!=b||!a.addEventListener||"onmouseenter"in document);var g=f?function(h){h=new ye(h);if(!oc(h.relatedTarget,function(k){return k==a}))return h.currentTarget=a,h.type=b,c.call(a,h)}:function(h){h=new ye(h);
h.currentTarget=a;return c.call(a,h)};
g=te(g);a.addEventListener?("mouseenter"==b&&f?b="mouseover":"mouseleave"==b&&f?b="mouseout":"mousewheel"==b&&"MozBoxSizing"in document.documentElement.style&&(b="MozMousePixelScroll"),Ce()||"boolean"===typeof d?a.addEventListener(b,g,d):a.addEventListener(b,g,!!d.capture)):a.attachEvent("on"+b,g);pb[e]=[a,b,c,g,d]}}}
;function Ee(a,b){"function"===typeof a&&(a=te(a));return window.setTimeout(a,b)}
function Fe(a){"function"===typeof a&&(a=te(a));return window.setInterval(a,250)}
;function Ge(a){var b=[];nb(a,function(c,d){var e=encodeURIComponent(String(d)),f;Array.isArray(c)?f=c:f=[c];H(f,function(g){""==g?b.push(e):b.push(e+"="+encodeURIComponent(String(g)))})});
return b.join("&")}
function He(a){"?"==a.charAt(0)&&(a=a.substr(1));a=a.split("&");for(var b={},c=0,d=a.length;c<d;c++){var e=a[c].split("=");if(1==e.length&&e[0]||2==e.length)try{var f=decodeURIComponent((e[0]||"").replace(/\+/g," ")),g=decodeURIComponent((e[1]||"").replace(/\+/g," "));f in b?Array.isArray(b[f])?lb(b[f],g):b[f]=[b[f],g]:b[f]=g}catch(k){if("q"!=e[0]){var h=Error("Error decoding URL component");h.params={key:e[0],value:e[1]};ue(h)}}}return b}
function Ie(a,b,c){var d=a.split("#",2);a=d[0];d=1<d.length?"#"+d[1]:"";var e=a.split("?",2);a=e[0];e=He(e[1]||"");for(var f in b)!c&&null!==e&&f in e||(e[f]=b[f]);b=a;a=tc(e);a?(c=b.indexOf("#"),0>c&&(c=b.length),f=b.indexOf("?"),0>f||f>c?(f=c,e=""):e=b.substring(f+1,c),b=[b.substr(0,f),e,b.substr(c)],c=b[1],b[1]=a?c?c+"&"+a:a:c,a=b[0]+(b[1]?"?"+b[1]:"")+b[2]):a=b;return a+d}
;var Je={};function Ke(a){return Je[a]||(Je[a]=String(a).replace(/\-([a-z])/g,function(b,c){return c.toUpperCase()}))}
;var Le={},Me=[],Zd=new O,Ne={};function Oe(){for(var a=u(Me),b=a.next();!b.done;b=a.next())b=b.value,b()}
function Pe(a,b){b||(b=document);var c=kb(b.getElementsByTagName("yt:"+a));var d="yt-"+a;var e=b||document;d=e.querySelectorAll&&e.querySelector?e.querySelectorAll("."+d):mc(d,b);d=kb(d);return jb(c,d)}
function R(a,b){var c;"yt:"==a.tagName.toLowerCase().substr(0,3)?c=a.getAttribute(b):c=a?a.dataset?a.dataset[Ke(b)]:a.getAttribute("data-"+b):null;return c}
function Qe(a,b){Zd.J.apply(Zd,arguments)}
;function Re(a){this.g=a||{};this.h=this.f=!1;a=document.getElementById("www-widgetapi-script");if(this.f=!!("https:"==document.location.protocol||a&&0==a.src.indexOf("https:"))){a=[this.g,window.YTConfig||{}];for(var b=0;b<a.length;b++)a[b].host&&(a[b].host=a[b].host.replace("http://","https://"))}}
function S(a,b){for(var c=[a.g,window.YTConfig||{}],d=0;d<c.length;d++){var e=c[d][b];if(void 0!=e)return e}return null}
function Se(a,b,c){Te||(Te={},De(window,"message",Za(a.i,a)));Te[c]=b}
Re.prototype.i=function(a){if(a.origin==S(this,"host")||a.origin==S(this,"host").replace(/^http:/,"https:")){try{var b=JSON.parse(a.data)}catch(c){return}this.h=!0;this.f||0!=a.origin.indexOf("https:")||(this.f=!0);if(a=Te[b.id])a.s=!0,a.s&&(H(a.m,a.S,a),a.m.length=0),a.ba(b)}};
var Te=null;function T(a){a=Ue(a);return"string"===typeof a&&"false"===a?!1:!!a}
function Ve(a,b){var c=Ue(a);return void 0===c&&void 0!==b?b:Number(c||0)}
function Ue(a){var b=P("EXPERIMENTS_FORCED_FLAGS",{});return void 0!==b[a]?b[a]:P("EXPERIMENT_FLAGS",{})[a]}
;function We(){}
function Xe(a,b){return Ye(a,0,b)}
;function Ze(){}
sa(Ze,We);function Ye(a,b,c){isNaN(c)&&(c=void 0);var d=B("yt.scheduler.instance.addJob");return d?d(a,b,c):void 0===c?(a(),NaN):Ee(a,c||0)}
Ze.prototype.start=function(){var a=B("yt.scheduler.instance.start");a&&a()};
Ze.f=void 0;Ze.g=function(){Ze.f||(Ze.f=new Ze)};
Ze.g();var $e=A.ytPubsubPubsubInstance||new O,af=A.ytPubsubPubsubSubscribedKeys||{},bf=A.ytPubsubPubsubTopicToKeys||{},cf=A.ytPubsubPubsubIsSynchronous||{};O.prototype.subscribe=O.prototype.subscribe;O.prototype.unsubscribeByKey=O.prototype.L;O.prototype.publish=O.prototype.J;O.prototype.clear=O.prototype.clear;D("ytPubsubPubsubInstance",$e);D("ytPubsubPubsubTopicToKeys",bf);D("ytPubsubPubsubIsSynchronous",cf);D("ytPubsubPubsubSubscribedKeys",af);var df=window,U=df.ytcsi&&df.ytcsi.now?df.ytcsi.now:df.performance&&df.performance.timing&&df.performance.now&&df.performance.timing.navigationStart?function(){return df.performance.timing.navigationStart+df.performance.now()}:function(){return(new Date).getTime()};var ef=Ve("initial_gel_batch_timeout",1E3),ff=Math.pow(2,16)-1,Of=null,Pf=0,Qf=void 0,Rf=0,Sf=0,Tf=0,Uf=!0,Vf=A.ytLoggingTransportLogPayloadsQueue_||{};D("ytLoggingTransportLogPayloadsQueue_",Vf);var Wf=A.ytLoggingTransportGELQueue_||new Map;D("ytLoggingTransportGELQueue_",Wf);var Xf=A.ytLoggingTransportTokensToCttTargetIds_||{};D("ytLoggingTransportTokensToCttTargetIds_",Xf);
function Yf(){window.clearTimeout(Rf);window.clearTimeout(Sf);Sf=0;Qf&&Qf.isReady()?(Zf(Wf),"log_event"in Vf&&Zf(Object.entries(Vf.log_event)),Wf.clear(),delete Vf.log_event):$f()}
function $f(){T("web_gel_timeout_cap")&&!Sf&&(Sf=Ee(Yf,6E4));window.clearTimeout(Rf);var a=P("LOGGING_BATCH_TIMEOUT",Ve("web_gel_debounce_ms",1E4));T("shorten_initial_gel_batch_timeout")&&Uf&&(a=ef);Rf=Ee(Yf,a)}
function Zf(a){var b=Qf,c=Math.round(U());a=u(a);for(var d=a.next();!d.done;d=a.next()){var e=u(d.value);d=e.next().value;var f=e.next().value;e=rb({context:ag(b.f||bg())});e.events=f;(f=Xf[d])&&cg(e,d,f);delete Xf[d];dg(e,c);eg(b,"log_event",e,{retry:!0,onSuccess:function(){Pf=Math.round(U()-c)}});
Uf=!1}}
function dg(a,b){a.requestTimeMs=String(b);T("unsplit_gel_payloads_in_logs")&&(a.unsplitGelPayloadsInLogs=!0);var c=P("EVENT_ID",void 0);if(c){var d=P("BATCH_CLIENT_COUNTER",void 0)||0;!d&&T("web_client_counter_random_seed")&&(d=Math.floor(Math.random()*ff/2));d++;d>ff&&(d=1);qe("BATCH_CLIENT_COUNTER",d);c={serializedEventId:c,clientCounter:String(d)};a.serializedClientEventId=c;Of&&Pf&&T("log_gel_rtt_web")&&(a.previousBatchInfo={serializedClientEventId:Of,roundtripMs:String(Pf)});Of=c;Pf=0}}
function cg(a,b,c){if(c.videoId)var d="VIDEO";else if(c.playlistId)d="PLAYLIST";else return;a.credentialTransferTokenTargetId=c;a.context=a.context||{};a.context.user=a.context.user||{};a.context.user.credentialTransferTokens=[{token:b,scope:d}]}
;var fg=A.ytLoggingGelSequenceIdObj_||{};D("ytLoggingGelSequenceIdObj_",fg);function gg(a){var b=hg;a=void 0===a?B("yt.ads.biscotti.lastId_")||"":a;b=Object.assign(ig(b),jg(b));b.ca_type="image";a&&(b.bid=a);return b}
function ig(a){var b={};b.dt=Ec;b.flash="0";a:{try{var c=a.f.top.location.href}catch(f){a=2;break a}a=c?c===a.g.location.href?0:1:2}b=(b.frm=a,b);b.u_tz=-(new Date).getTimezoneOffset();var d=void 0===d?K:d;try{var e=d.history.length}catch(f){e=0}b.u_his=e;b.u_java=!!K.navigator&&"unknown"!==typeof K.navigator.javaEnabled&&!!K.navigator.javaEnabled&&K.navigator.javaEnabled();K.screen&&(b.u_h=K.screen.height,b.u_w=K.screen.width,b.u_ah=K.screen.availHeight,b.u_aw=K.screen.availWidth,b.u_cd=K.screen.colorDepth);
K.navigator&&K.navigator.plugins&&(b.u_nplug=K.navigator.plugins.length);K.navigator&&K.navigator.mimeTypes&&(b.u_nmime=K.navigator.mimeTypes.length);return b}
function jg(a){var b=a.f;try{var c=b.screenX;var d=b.screenY}catch(n){}try{var e=b.outerWidth;var f=b.outerHeight}catch(n){}try{var g=b.innerWidth;var h=b.innerHeight}catch(n){}b=[b.screenLeft,b.screenTop,c,d,b.screen?b.screen.availWidth:void 0,b.screen?b.screen.availTop:void 0,e,f,g,h];c=a.f.top;try{var k=(c||window).document,l="CSS1Compat"==k.compatMode?k.documentElement:k.body;var m=(new lc(l.clientWidth,l.clientHeight)).round()}catch(n){m=new lc(-12245933,-12245933)}k=m;m={};l=new Kc;A.SVGElement&&
A.document.createElementNS&&l.set(0);c=zc();c["allow-top-navigation-by-user-activation"]&&l.set(1);c["allow-popups-to-escape-sandbox"]&&l.set(2);A.crypto&&A.crypto.subtle&&l.set(3);A.TextDecoder&&A.TextEncoder&&l.set(4);l=Lc(l);m.bc=l;m.bih=k.height;m.biw=k.width;m.brdim=b.join();a=a.g;return m.vis={visible:1,hidden:2,prerender:3,preview:4,unloaded:5}[a.visibilityState||a.webkitVisibilityState||a.mozVisibilityState||""]||0,m.wgl=!!K.WebGLRenderingContext,m}
var hg=new function(){var a=window.document;this.f=window;this.g=a};
D("yt.ads_.signals_.getAdSignalsString",function(a){return Ge(gg(a))});C();var kg="XMLHttpRequest"in A?function(){return new XMLHttpRequest}:null;
function lg(){if(!kg)return null;var a=kg();return"open"in a?a:null}
;var mg={Authorization:"AUTHORIZATION","X-Goog-Visitor-Id":"SANDBOXED_VISITOR_ID","X-YouTube-Client-Name":"INNERTUBE_CONTEXT_CLIENT_NAME","X-YouTube-Client-Version":"INNERTUBE_CONTEXT_CLIENT_VERSION","X-YouTube-Device":"DEVICE","X-Youtube-Identity-Token":"ID_TOKEN","X-YouTube-Page-CL":"PAGE_CL","X-YouTube-Page-Label":"PAGE_BUILD_LABEL","X-YouTube-Variants-Checksum":"VARIANTS_CHECKSUM"},ng="app debugcss debugjs expflag force_ad_params force_viral_ad_response_params forced_experiments innertube_snapshots innertube_goldens internalcountrycode internalipoverride absolute_experiments conditional_experiments sbb sr_bns_address".split(" "),
og=!1;
function pg(a,b){b=void 0===b?{}:b;if(!c)var c=window.location.href;var d=a.match(pc)[1]||null,e=L(a);d&&e?(d=c,c=a.match(pc),d=d.match(pc),c=c[3]==d[3]&&c[1]==d[1]&&c[4]==d[4]):c=e?L(c)==e&&(Number(c.match(pc)[4]||null)||null)==(Number(a.match(pc)[4]||null)||null):!0;d=T("web_ajax_ignore_global_headers_if_set");for(var f in mg)e=P(mg[f]),!e||!c&&L(a)||d&&void 0!==b[f]||(b[f]=e);if(c||!L(a))b["X-YouTube-Utc-Offset"]=String(-(new Date).getTimezoneOffset());(c||!L(a))&&(f="undefined"!=typeof Intl?(new Intl.DateTimeFormat).resolvedOptions().timeZone:
null)&&(b["X-YouTube-Time-Zone"]=f);if(c||!L(a))b["X-YouTube-Ad-Signals"]=Ge(gg(void 0));return b}
function qg(a){var b=window.location.search,c=L(a),d=qc(a.match(pc)[5]||null);d=(c=c&&(c.endsWith("youtube.com")||c.endsWith("youtube-nocookie.com")))&&d&&d.startsWith("/api/");if(!c||d)return a;var e=He(b),f={};H(ng,function(g){e[g]&&(f[g]=e[g])});
return Ie(a,f||{},!1)}
function rg(a,b){if(window.fetch&&"XML"!=b.format){var c={method:b.method||"GET",credentials:"same-origin"};b.headers&&(c.headers=b.headers);a=sg(a,b);var d=tg(a,b);d&&(c.body=d);b.withCredentials&&(c.credentials="include");var e=!1,f;fetch(a,c).then(function(g){if(!e){e=!0;f&&window.clearTimeout(f);var h=g.ok,k=function(l){l=l||{};var m=b.context||A;h?b.onSuccess&&b.onSuccess.call(m,l,g):b.onError&&b.onError.call(m,l,g);b.P&&b.P.call(m,l,g)};
"JSON"==(b.format||"JSON")&&(h||400<=g.status&&500>g.status)?g.json().then(k,function(){k(null)}):k(null)}});
b.Y&&0<b.timeout&&(f=Ee(function(){e||(e=!0,window.clearTimeout(f),b.Y.call(b.context||A))},b.timeout))}else ug(a,b)}
function ug(a,b){var c=b.format||"JSON";a=sg(a,b);var d=tg(a,b),e=!1,f=vg(a,function(k){if(!e){e=!0;h&&window.clearTimeout(h);a:switch(k&&"status"in k?k.status:-1){case 200:case 201:case 202:case 203:case 204:case 205:case 206:case 304:var l=!0;break a;default:l=!1}var m=null,n=400<=k.status&&500>k.status,r=500<=k.status&&600>k.status;if(l||n||r)m=wg(a,c,k,b.Ea);if(l)a:if(k&&204==k.status)l=!0;else{switch(c){case "XML":l=0==parseInt(m&&m.return_code,10);break a;case "RAW":l=!0;break a}l=!!m}m=m||
{};n=b.context||A;l?b.onSuccess&&b.onSuccess.call(n,k,m):b.onError&&b.onError.call(n,k,m);b.P&&b.P.call(n,k,m)}},b.method,d,b.headers,b.responseType,b.withCredentials);
if(b.I&&0<b.timeout){var g=b.I;var h=Ee(function(){e||(e=!0,f.abort(),window.clearTimeout(h),g.call(b.context||A,f))},b.timeout)}}
function sg(a,b){b.Ia&&(a=document.location.protocol+"//"+document.location.hostname+(document.location.port?":"+document.location.port:"")+a);var c=P("XSRF_FIELD_NAME",void 0),d=b.oa;d&&(d[c]&&delete d[c],a=Ie(a,d||{},!0));return a}
function tg(a,b){var c=P("XSRF_FIELD_NAME",void 0),d=P("XSRF_TOKEN",void 0),e=b.postBody||"",f=b.u,g=P("XSRF_FIELD_NAME",void 0),h;b.headers&&(h=b.headers["Content-Type"]);b.Ha||L(a)&&!b.withCredentials&&L(a)!=document.location.hostname||"POST"!=b.method||h&&"application/x-www-form-urlencoded"!=h||b.u&&b.u[g]||(f||(f={}),f[c]=d);f&&"string"===typeof e&&(e=He(e),tb(e,f),e=b.aa&&"JSON"==b.aa?JSON.stringify(e):tc(e));if(!(c=e)&&(c=f)){a:{for(var k in f){f=!1;break a}f=!0}c=!f}!og&&c&&"POST"!=b.method&&
(og=!0,ue(Error("AJAX request with postData should use POST")));return e}
function wg(a,b,c,d){var e=null;switch(b){case "JSON":try{var f=c.responseText}catch(g){throw d=Error("Error reading responseText"),d.params=a,ve(d),g;}a=c.getResponseHeader("Content-Type")||"";f&&0<=a.indexOf("json")&&(e=JSON.parse(f));break;case "XML":if(a=(a=c.responseXML)?xg(a):null)e={},H(a.getElementsByTagName("*"),function(g){e[g.tagName]=yg(g)})}d&&zg(e);
return e}
function zg(a){if(Ta(a))for(var b in a){var c;(c="html_content"==b)||(c=b.length-5,c=0<=c&&b.indexOf("_html",c)==c);if(c){c=b;var d=a[b];if(void 0===ub){var e=null;var f=A.trustedTypes;if(f&&f.createPolicy){try{e=f.createPolicy("goog#html",{createHTML:$a,createScript:$a,createScriptURL:$a})}catch(g){A.console&&A.console.error(g.message)}ub=e}else ub=e}(e=ub)&&e.createHTML(d);a[c]=new Fb}else zg(a[b])}}
function xg(a){return a?(a=("responseXML"in a?a.responseXML:a).getElementsByTagName("root"))&&0<a.length?a[0]:null:null}
function yg(a){var b="";H(a.childNodes,function(c){b+=c.nodeValue});
return b}
function vg(a,b,c,d,e,f,g){function h(){4==(k&&"readyState"in k?k.readyState:0)&&b&&te(b)(k)}
c=void 0===c?"GET":c;d=void 0===d?"":d;var k=lg();if(!k)return null;"onloadend"in k?k.addEventListener("loadend",h,!1):k.onreadystatechange=h;T("debug_forward_web_query_parameters")&&(a=qg(a));k.open(c,a,!0);f&&(k.responseType=f);g&&(k.withCredentials=!0);c="POST"==c&&(void 0===window.FormData||!(d instanceof FormData));if(e=pg(a,e))for(var l in e)k.setRequestHeader(l,e[l]),"content-type"==l.toLowerCase()&&(c=!1);c&&k.setRequestHeader("Content-Type","application/x-www-form-urlencoded");k.send(d);
return k}
;function Ag(){for(var a={},b=u(Object.entries(He(P("DEVICE","")))),c=b.next();!c.done;c=b.next()){var d=u(c.value);c=d.next().value;d=d.next().value;"cbrand"===c?a.deviceMake=d:"cmodel"===c?a.deviceModel=d:"cbr"===c?a.browserName=d:"cbrver"===c?a.browserVersion=d:"cos"===c?a.osName=d:"cosver"===c?a.osVersion=d:"cplatform"===c&&(a.platform=d)}return a}
;function Bg(){return"INNERTUBE_API_KEY"in pe&&"INNERTUBE_API_VERSION"in pe}
function bg(){return{innertubeApiKey:P("INNERTUBE_API_KEY",void 0),innertubeApiVersion:P("INNERTUBE_API_VERSION",void 0),ga:P("INNERTUBE_CONTEXT_CLIENT_CONFIG_INFO"),ha:P("INNERTUBE_CONTEXT_CLIENT_NAME","WEB"),innertubeContextClientVersion:P("INNERTUBE_CONTEXT_CLIENT_VERSION",void 0),ja:P("INNERTUBE_CONTEXT_HL",void 0),ia:P("INNERTUBE_CONTEXT_GL",void 0),ka:P("INNERTUBE_HOST_OVERRIDE",void 0)||"",ma:!!P("INNERTUBE_USE_THIRD_PARTY_AUTH",!1),la:!!P("INNERTUBE_OMIT_API_KEY_WHEN_AUTH_HEADER_IS_PRESENT",
!1)}}
function ag(a){a={client:{hl:a.ja,gl:a.ia,clientName:a.ha,clientVersion:a.innertubeContextClientVersion,configInfo:a.ga}};var b=window.devicePixelRatio;b&&1!=b&&(a.client.screenDensityFloat=String(b));b=P("EXPERIMENTS_TOKEN","");""!==b&&(a.client.experimentsToken=b);b=[];var c=P("EXPERIMENTS_FORCED_FLAGS",{});for(d in c)b.push({key:d,value:String(c[d])});var d=P("EXPERIMENT_FLAGS",{});for(var e in d)e.startsWith("force_")&&void 0===c[e]&&b.push({key:e,value:String(d[e])});0<b.length&&(a.request={internalExperimentFlags:b});
P("DELEGATED_SESSION_ID")&&!T("pageid_as_header_web")&&(a.user={onBehalfOfUser:P("DELEGATED_SESSION_ID")});a.client=Object.assign(a.client,Ag());return a}
function Cg(a,b,c){c=void 0===c?{}:c;var d={"X-Goog-Visitor-Id":c.visitorData||P("VISITOR_DATA","")};if(b&&b.includes("www.youtube-nocookie.com"))return d;(b=c.Ca||P("AUTHORIZATION"))||(a?b="Bearer "+B("gapi.auth.getToken")().Ba:b=Jc([]));b&&(d.Authorization=b,d["X-Goog-AuthUser"]=P("SESSION_INDEX",0),T("pageid_as_header_web")&&(d["X-Goog-PageId"]=P("DELEGATED_SESSION_ID")));return d}
;function Dg(a){a=Object.assign({},a);delete a.Authorization;var b=Jc();if(b){var c=new ud;c.update(P("INNERTUBE_API_KEY",void 0));c.update(b);b=c.digest();c=3;Qa(b);void 0===c&&(c=0);if(!ec){ec={};for(var d="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789".split(""),e=["+/=","+/","-_=","-_.","-_"],f=0;5>f;f++){var g=d.concat(e[f].split(""));dc[f]=g;for(var h=0;h<g.length;h++){var k=g[h];void 0===ec[k]&&(ec[k]=h)}}}c=dc[c];d=[];for(e=0;e<b.length;e+=3){var l=b[e],m=(f=e+1<b.length)?
b[e+1]:0;k=(g=e+2<b.length)?b[e+2]:0;h=l>>2;l=(l&3)<<4|m>>4;m=(m&15)<<2|k>>6;k&=63;g||(k=64,f||(m=64));d.push(c[h],c[l],c[m]||"",c[k]||"")}a.hash=d.join("")}return a}
function Eg(a){a=Dg(a);var b=new ud;b.update(JSON.stringify(a,Object.keys(a).sort()));a=b.digest();b="";for(var c=0;c<a.length;c++)b+="0123456789ABCDEF".charAt(Math.floor(a[c]/16))+"0123456789ABCDEF".charAt(a[c]%16);return b}
;function Fg(){var a=new ie;(a=a.isAvailable()?new oe(a,"yt.innertube"):null)||(a=new je("yt.innertube"),a=a.isAvailable()?a:null);this.f=a?new ee(a):null;this.g=document.domain||window.location.hostname}
Fg.prototype.set=function(a,b,c,d){c=c||31104E3;this.remove(a);if(this.f)try{this.f.set(a,b,C()+1E3*c);return}catch(f){}var e="";if(d)try{e=escape(Fd(b))}catch(f){return}else e=escape(b);b=this.g;kc.set(""+a,e,{V:c,path:"/",domain:void 0===b?"youtube.com":b,secure:!1})};
Fg.prototype.get=function(a,b){var c=void 0,d=!this.f;if(!d)try{c=this.f.get(a)}catch(e){d=!0}if(d&&(c=kc.get(""+a,void 0))&&(c=unescape(c),b))try{c=JSON.parse(c)}catch(e){this.remove(a),c=void 0}return c};
Fg.prototype.remove=function(a){this.f&&this.f.remove(a);var b=this.g;kc.remove(""+a,"/",void 0===b?"youtube.com":b)};var Gg;function Hg(){Gg||(Gg=new Fg);return Gg}
function Ig(a,b,c,d){if(d)return null;d=Hg().get("nextId",!0)||1;var e=Hg().get("requests",!0)||{};e[d]={method:a,request:b,authState:Dg(c),requestTime:Math.round(U())};Hg().set("nextId",d+1,86400,!0);Hg().set("requests",e,86400,!0);return d}
function Jg(a){var b=Hg().get("requests",!0)||{};delete b[a];Hg().set("requests",b,86400,!0)}
function Kg(a){var b=Hg().get("requests",!0);if(b){for(var c in b){var d=b[c];if(!(6E4>Math.round(U())-d.requestTime)){var e=d.authState,f=Dg(Cg(!1));qb(e,f)&&(e=d.request,"requestTimeMs"in e&&(e.requestTimeMs=Math.round(U())),eg(a,d.method,e,{}));delete b[c]}}Hg().set("requests",b,86400,!0)}}
;function Lg(){}
;var Mg=bc||cc;function Ng(a){if(!a)throw Error();throw a;}
function Og(a){return a}
function W(a){var b=this;this.g=a;this.state={status:"PENDING"};this.f=[];this.onRejected=[];this.g(function(c){if("PENDING"===b.state.status){b.state={status:"FULFILLED",value:c};c=u(b.f);for(var d=c.next();!d.done;d=c.next())d=d.value,d()}},function(c){if("PENDING"===b.state.status){b.state={status:"REJECTED",
reason:c};c=u(b.onRejected);for(var d=c.next();!d.done;d=c.next())d=d.value,d()}})}
W.all=function(a){return new W(function(b,c){var d=[],e=a.length;0===e&&b(d);for(var f={D:0};f.D<a.length;f={D:f.D},++f.D)Pg(W.resolve(a[f.D]).then(function(g){return function(h){d[g.D]=h;e--;0===e&&b(d)}}(f)),function(g){c(g)})})};
W.resolve=function(a){return new W(function(b,c){a instanceof W?a.then(b,c):b(a)})};
W.reject=function(a){return new W(function(b,c){c(a)})};
W.prototype.then=function(a,b){var c=this,d=null!==a&&void 0!==a?a:Og,e=null!==b&&void 0!==b?b:Ng;return new W(function(f,g){"PENDING"===c.state.status?(c.f.push(function(){Qg(c,c,d,f,g)}),c.onRejected.push(function(){Rg(c,c,e,f,g)})):"FULFILLED"===c.state.status?Qg(c,c,d,f,g):"REJECTED"===c.state.status&&Rg(c,c,e,f,g)})};
function Pg(a,b){a.then(void 0,b)}
function Qg(a,b,c,d,e){try{if("FULFILLED"!==a.state.status)throw Error("calling handleResolve before the promise is fulfilled.");var f=c(a.state.value);f instanceof W?Sg(a,b,f,d,e):d(f)}catch(g){e(g)}}
function Rg(a,b,c,d,e){try{if("REJECTED"!==a.state.status)throw Error("calling handleReject before the promise is rejected.");var f=c(a.state.reason);f instanceof W?Sg(a,b,f,d,e):d(f)}catch(g){e(g)}}
function Sg(a,b,c,d,e){b===c?e(new TypeError("Circular promise chain detected.")):c.then(function(f){f instanceof W?Sg(a,b,f,d,e):d(f)},function(f){e(f)})}
;function Tg(){var a=Error.call(this,"Transaction was aborted");this.message=a.message;"stack"in a&&(this.stack=a.stack);Object.setPrototypeOf(this,Tg.prototype)}
sa(Tg,Error);function Ug(a){return a instanceof DOMException?"VersionError"===a.name:"DOMError"in self&&a instanceof DOMError?"VersionError"===a.name:a instanceof Object&&"message"in a?"An attempt was made to open a database using a lower version than the existing version."===a.message:!1}
function Vg(a,b){return new W(function(c,d){function e(){var f=a?b(a):null;f?f.then(function(g){a=g;e()},d):c()}
e()})}
;function Wg(a){return new M(function(b,c){function d(){c(a.error);f()}
function e(){b(a.result);f()}
function f(){a.removeEventListener("success",e);a.removeEventListener("error",d)}
a.addEventListener("success",e);a.addEventListener("error",d)})}
function X(a){return new W(function(b,c){function d(){c(a.error);f()}
function e(){b(a.result);f()}
function f(){a.removeEventListener("success",e);a.removeEventListener("error",d)}
a.addEventListener("success",e);a.addEventListener("error",d)})}
;function Xg(a){this.f=a}
p=Xg.prototype;p.add=function(a,b,c){return Yg(this,[a],"readwrite",function(d){return Zg(d,a).add(b,c)})};
p.clear=function(a){return Yg(this,[a],"readwrite",function(b){return Zg(b,a).clear()})};
p.close=function(){this.f.close()};
p.count=function(a,b){return Yg(this,[a],"readonly",function(c){return Zg(c,a).count(b)})};
function $g(a,b,c){a=a.f.createObjectStore(b,c);return new ah(a)}
p["delete"]=function(a,b){return Yg(this,[a],"readwrite",function(c){return Zg(c,a)["delete"](b)})};
p.get=function(a,b){return Yg(this,[a],"readwrite",function(c){return Zg(c,a).get(b)})};
function bh(a,b){return Yg(a,["databases"],"readwrite",function(c){return ch(Zg(c,"databases"),b,void 0)})}
function Yg(a,b,c,d){c=void 0===c?"readonly":c;return new M(function(e,f){var g=a.f.transaction(b,c),h=new dh(g,b);Pg(d(h).then(function(k){Nd(eh(h).then(function(){e(k)}),function(l){f(l)})}),function(k){f(k)})})}
function ah(a){this.f=a}
p=ah.prototype;p.add=function(a,b){var c=b?this.f.add(a,b):this.f.add(a);return X(c)};
p.clear=function(){return X(this.f.clear()).then(function(){})};
p.count=function(a){a=a?this.f.count(a):this.f.count();return X(a)};
p["delete"]=function(a){return X(this.f["delete"](a))};
p.get=function(a){return X(this.f.get(a))};
p.index=function(a){return new fh(this.f.index(a))};
p.getName=function(){return this.f.name};
function ch(a,b,c){a=c?a.f.put(b,c):a.f.put(b);return X(a)}
function dh(a){var b=this;this.f=a;this.g=new Map;this.aborted=!1;this.done=new M(function(c,d){b.f.addEventListener("complete",function(){c()});
b.f.addEventListener("error",function(e){e.currentTarget===e.target&&d(b.f.error)});
b.f.addEventListener("abort",function(){var e=b.f.error;e?("QuotaExceededError"===e.name?ve(Error("The current transaction exceeded its quota limitations.")):"UnknownError"===e.name&&ve(Error("The current transaction may have failed because of exceeding quota limitations.")),d(e)):d(new Tg)})})}
dh.prototype.abort=function(){this.f.abort();this.aborted=!0};
function eh(a){var b=a.f;b.commit&&!a.aborted&&b.commit();return a.done}
function Zg(a,b){var c=a.f.objectStore(b),d=a.g.get(c);d||(d=new ah(c),a.g.set(c,d));return d}
function fh(a){this.f=a}
fh.prototype.count=function(a){a=a?this.f.count(a):this.f.count();return X(a)};
fh.prototype.get=function(a){return X(this.f.get(a))};
function gh(a,b,c){a=a.f.openCursor(void 0===b.query?null:b.query,void 0===b.direction?"next":b.direction);return hh(a).then(function(d){return Vg(d,c)})}
function ih(a,b){this.request=a;this.f=b}
function hh(a){return X(a).then(function(b){return null===b?null:new ih(a,b)})}
ih.prototype["delete"]=function(){return X(this.f["delete"]()).then(function(){})};
ih.prototype.update=function(a){return X(this.f.update(a))};function jh(a,b,c){function d(){m||(m=new Xg(f.result));return m}
var e=dh;var f=void 0!==b?self.indexedDB.open(a,b):self.indexedDB.open(a);var g=c.blocked,h=c.blocking,k=c.f,l=c.upgrade,m;l&&f.addEventListener("upgradeneeded",function(n){if(null===n.newVersion)throw Error("Invariant: newVersion on IDbVersionChangeEvent is null");if(null===f.transaction)throw Error("Invariant: transaction on IDbOpenDbRequest is null");var r=d(),q=new e(f.transaction);l(r,n.oldVersion,n.newVersion,q)});
g&&f.addEventListener("blocked",function(){g()});
return Wg(f).then(function(n){h&&n.addEventListener("versionchange",function(){h(d())});
k&&n.addEventListener("close",function(){k()});
return d()})}
function kh(a,b,c){c=void 0===c?{}:c;return jh(a,b,c)}
;var lh,mh,nh=["getAll","getAllKeys","getKey","openKeyCursor"],oh=["getAll","getAllKeys","getKey","openKeyCursor"];function ph(){var a;if(a=Mg)a=/WebKit\/([0-9]+)/.exec(I),a=!!(a&&600<=parseInt(a[1],10));a&&(a=/WebKit\/([0-9]+)/.exec(I),a=!(a&&602<=parseInt(a[1],10)));return a&&!T("ytidb_allow_on_ios_safari_v8_and_v9")?!1:!!self.indexedDB}
function qh(){return new Promise(function(a){setTimeout(a,50)})}
function rh(){return F(this,function b(){var c,d,e;return y(b,function(f){switch(f.f){case 1:if(!ph())return f["return"](!1);wa(f);d=!1;return x(f,kh("yt-idb-test-do-not-use",void 0,{blocking:function(){d=!0;c&&(c.close(),c=void 0)}}),5);
case 5:return c=f.g,e=kh("yt-idb-test-do-not-use",c.f.version+1).then(function(g){try{g.close()}catch(h){}}),x(f,Promise.race([e,
qh()]),6);case 6:return f["return"](d);case 3:ya(f);if(c)try{c.close()}catch(g){}za(f);break;case 2:return xa(f),f["return"](!1)}})})}
function sh(){return void 0!==lh?Kd(lh):new M(function(a){rh().then(function(b){lh=b;a(b)})})}
function th(){return void 0!==mh?Kd(mh):sh().then(function(a){if(!a)return!1;var b=u(nh);for(a=b.next();!a.done;a=b.next())if(!IDBObjectStore.prototype[a.value])return!1;b=u(oh);for(a=b.next();!a.done;a=b.next())if(!IDBIndex.prototype[a.value])return!1;return IDBObjectStore.prototype.getKey?!0:!1}).then(function(a){return mh=a})}
;var uh;function vh(){function a(b){b.close();uh=void 0}
uh||(uh=Nd(kh("YtIdbMeta",1,{blocking:a,upgrade:function(b,c){1>c&&$g(b,"databases",{keyPath:"actualName"})}}),function(b){return Ug(b)?kh("YtIdbMeta",void 0,{blocking:a}):Ld(b)}));
return uh}
function wh(a){return vh().then(function(b){return b.get("databases",a.actualName).then(function(c){if(c?a.actualName!==c.actualName||a.publicName!==c.publicName||a.userIdentifier!==c.userIdentifier||a.signedIn!==c.signedIn:1)return bh(b,a)})})}
function xh(a){return vh().then(function(b){return b["delete"]("databases",a)})}
;function yh(a){try{var b={actualName:"LogsDataBase",publicName:"LogsDataBase",userIdentifier:void 0,signedIn:!1}}catch(c){return Ld(c)}return Nd(wh(b).then(function(){return a(b)}),function(c){return Nd(xh(b.actualName),function(){}).then(function(){return Ld(c)})})}
function zh(a,b){b=void 0===b?{}:b;return yh(function(c){return kh(c.actualName,a,b)})}
;function Ah(){W.call(this,function(){});
throw Error("Not allowed to instantiate the thennable outside of the core library.");}
sa(Ah,W);Ah.reject=W.reject;Ah.resolve=W.resolve;Ah.all=W.all;var Bh;function Ch(){return F(this,function b(){return y(b,function(c){if(!Bh)try{Bh=zh(1,{upgrade:function(d,e){1>e&&($g(d,"LogsRequestsStore",{keyPath:"id",autoIncrement:!0}).f.createIndex("newRequest",["status","authHash","timestamp"],{unique:!1}),$g(d,"sapisid"))}})}catch(d){if(!Ug(d))return ue(d),c["return"](Promise.reject(d));
Bh=zh()}return c["return"](Bh)})})}
function Dh(a){return F(this,function c(){var d,e,f;return y(c,function(g){if(1==g.f)return x(g,Eh(),2);if(3!=g.f)return d=g.g,x(g,Ch(),3);e=g.g;f=Object.assign(Object.assign({},a),{options:JSON.parse(JSON.stringify(a.options)),authHash:d});return g["return"](e.add("LogsRequestsStore",f))})})}
function Fh(){return F(this,function b(){var c,d,e,f,g,h;return y(b,function(k){switch(k.f){case 1:return x(k,Eh(),2);case 2:return c=k.g,d=["NEW",c,0],e=["NEW",c,U()],f=IDBKeyRange.bound(d,e),x(k,Ch(),3);case 3:return g=k.g,h=void 0,x(k,Yg(g,["LogsRequestsStore"],"readwrite",function(l){return gh(Zg(l,"LogsRequestsStore").index("newRequest"),{query:f,direction:"prev"},function(m){m.f.value&&(h=m.f.value,h.status="QUEUED",m.update(h))})}),4);
case 4:return k["return"](h)}})})}
function Gh(a){return F(this,function c(){var d;return y(c,function(e){if(1==e.f)return x(e,Ch(),2);d=e.g;return e["return"](Yg(d,["LogsRequestsStore"],"readwrite",function(f){var g=Zg(f,"LogsRequestsStore");return g.get(a).then(function(h){if(h)return h.status="QUEUED",ch(g,h).then(function(){return h})})}))})})}
function Hh(a){return F(this,function c(){var d;return y(c,function(e){if(1==e.f)return x(e,Ch(),2);d=e.g;return e["return"](Yg(d,["LogsRequestsStore"],"readwrite",function(f){var g=Zg(f,"LogsRequestsStore");return g.get(a).then(function(h){return h?(h.status="NEW",h.sendCount+=1,ch(g,h).then(function(){return h})):Ah.resolve(void 0)})}))})})}
function Ih(a){return F(this,function c(){var d;return y(c,function(e){if(1==e.f)return x(e,Ch(),2);d=e.g;return e["return"](d["delete"]("LogsRequestsStore",a))})})}
function Eh(){return F(this,function b(){var c;return y(b,function(d){if(1==d.f){Lg.f||(Lg.f=new Lg);var e={};var f=Jc([]);f&&(e.Authorization=f,e["X-Goog-AuthUser"]=P("SESSION_INDEX",0),"INNERTUBE_HOST_OVERRIDE"in pe||(e["X-Origin"]=window.location.origin),T("pageid_as_header_web")&&"DELEGATED_SESSION_ID"in pe&&(e["X-Goog-PageId"]=P("DELEGATED_SESSION_ID")));e=Kd(e);return x(d,e,2)}c=d.g;return d["return"](Eg(c))})})}
;var Jh=Ve("network_polling_interval",3E4);function Kh(){this.i=0;this.f=window.navigator.onLine;Lh(this);Mh(this)}
function Nh(){Kh.f||(Kh.f=new Kh);return Kh.f}
function Oh(a){var b=Ph,c=Qh;(new M(function(d){a.g=d})).then(function(){b();
c&&(a.h=c)});
Rh(a)}
function Mh(a){window.addEventListener("online",function(){a.f=!0;a.g&&a.g()})}
function Lh(a){window.addEventListener("offline",function(){a.f=!1;a.h&&a.h()})}
function Rh(a){a.i||(Sh(a),window.navigator.onLine&&a.g&&a.g())}
function Sh(a){a.i=Xe(function(){window.navigator.onLine?(!1===a.f&&ue(Error("NetworkStatusManager missed online event.")),a.f=!0,a.g&&a.g()):(!0===a.f&&ue(Error("NetworkStatusManager missed offline event.")),a.f=!1,a.h&&a.h());Sh(a)},Jh)}
;var Th=Ve("networkless_throttle_timeout")||100,Uh=Ve("networkless_retry_attempts")||1,Vh=0;function Wh(a,b){b=void 0===b?{}:b;Xh().then(function(c){if(c&&!T("networkless_bypass_write")){var d={url:a,options:b,timestamp:U(),status:"NEW",sendCount:0};Dh(d).then(function(e){d.id=e;e=Nh();e.f&&!T("networkless_always_offline")?Yh(d):Oh(e)})["catch"](function(e){Yh(d);
ue(e)})}else ug(a,b)})}
function Ph(){var a=this;Vh||(Vh=Ye(function(){return F(a,function c(){var d;return y(c,function(e){if(1==e.f)return x(e,Fh(),2);if(3!=e.f)return d=e.g,d?x(e,Yh(d),3):(Qh(),e["return"]());Vh=0;Ph();e.f=0})})},1,Th))}
function Qh(){var a=Vh;if(!isNaN(a)){var b=B("yt.scheduler.instance.cancelJob");b?b(a):window.clearTimeout(a)}Vh=0}
function Yh(a){return F(this,function c(){var d;return y(c,function(e){switch(e.f){case 1:if(!a.id){e.B(2);break}return x(e,Gh(a.id),3);case 3:(d=e.g)?a=d:ve(Error("The request cannot be found in the database."));case 2:var f=a.timestamp;if(!(2592E6<=U()-f)){e.B(4);break}ve(Error("Networkless Logging: Stored logs request expired age limit"));if(!a.id){e.B(5);break}return x(e,Ih(a.id),5);case 5:return e["return"]();case 4:(a=Zh(a))&&ug(a.url,a.options),e.f=0}})})}
function Zh(a){var b=this,c=a.options.onError?a.options.onError:function(){};
a.options.onError=function(e,f){return F(b,function h(){return y(h,function(k){if(1==k.f)return a&&a.id?a.sendCount<Uh?x(k,Hh(a.id),6):x(k,Ih(a.id),2):k.B(2);2!=k.f&&Oh(Nh());c(e,f);k.f=0})})};
var d=a.options.onSuccess?a.options.onSuccess:function(){};
a.options.onSuccess=function(e,f){return F(b,function h(){return y(h,function(k){if(1==k.f)return a&&a.id?x(k,Ih(a.id),2):k.B(2);d(e,f);k.f=0})})};
return a}
function Xh(){return F(this,function b(){return y(b,function(c){return T("networkless_logging")?(2===Ve("networkless_ytidb_version")&&th().then(function(d){return d}),c["return"](ph())):c["return"](!1)})})}
;function $h(a,b){for(var c=[],d=1;d<arguments.length;++d)c[d-1]=arguments[d];d=Error.call(this,a);this.message=d.message;"stack"in d&&(this.stack=d.stack);this.args=[].concat(c instanceof Array?c:ia(u(c)))}
sa($h,Error);function ai(a){var b=this;this.f=null;a?this.f=a:Bg()&&(this.f=bg());Xe(function(){Kg(b)},5E3)}
ai.prototype.isReady=function(){!this.f&&Bg()&&(this.f=bg());return!!this.f};
function eg(a,b,c,d){!P("VISITOR_DATA")&&"visitor_id"!==b&&.01>Math.random()&&ve(new $h("Missing VISITOR_DATA when sending innertube request.",b,c,d));if(!a.isReady()){var e=new $h("innertube xhrclient not ready",b,c,d);ue(e);e.sampleWeight=0;throw e;}var f={headers:{"Content-Type":"application/json"},method:"POST",u:c,aa:"JSON",I:function(){d.I()},
Y:d.I,onSuccess:function(n,r){if(d.onSuccess)d.onSuccess(r)},
X:function(n){if(d.onSuccess)d.onSuccess(n)},
onError:function(n,r){if(d.onError)d.onError(r)},
Ja:function(n){if(d.onError)d.onError(n)},
timeout:d.timeout,withCredentials:!0},g="";(e=a.f.ka)&&(g=e);var h=a.f.ma||!1,k=Cg(h,g,d);Object.assign(f.headers,k);f.headers.Authorization&&!g&&(f.headers["x-origin"]=window.location.origin);e="/youtubei/"+a.f.innertubeApiVersion+"/"+b;var l={alt:"json"};a.f.la&&f.headers.Authorization||(l.key=a.f.innertubeApiKey);var m=Ie(""+g+e,l||{},!0);Xh().then(function(n){if(d.retry&&T("retry_web_logging_batches")&&"www.youtube-nocookie.com"!=g){if(T("networkless_gel")&&!n||!T("networkless_gel"))var r=Ig(b,
c,k,h);if(r){var q=f.onSuccess,v=f.X;f.onSuccess=function(w,G){Jg(r);q(w,G)};
c.X=function(w,G){Jg(r);v(w,G)}}}try{T("use_fetch_for_op_xhr")?rg(m,f):T("networkless_gel")&&d.retry?(f.method="POST",Wh(m,f)):(f.method="POST",f.u||(f.u={}),ug(m,f))}catch(w){if("InvalidAccessError"==w.name)r&&(Jg(r),r=0),ve(Error("An extension is blocking network request."));
else throw w;}r&&Xe(function(){Kg(a)},5E3)})}
;var bi=[{W:function(a){return"Cannot read property '"+a.key+"'"},
R:{TypeError:[{regexp:/Cannot read property '([^']+)' of (null|undefined)/,groups:["key","value"]},{regexp:/\u65e0\u6cd5\u83b7\u53d6\u672a\u5b9a\u4e49\u6216 (null|undefined) \u5f15\u7528\u7684\u5c5e\u6027\u201c([^\u201d]+)\u201d/,groups:["value","key"]},{regexp:/\uc815\uc758\ub418\uc9c0 \uc54a\uc74c \ub610\ub294 (null|undefined) \ucc38\uc870\uc778 '([^']+)' \uc18d\uc131\uc744 \uac00\uc838\uc62c \uc218 \uc5c6\uc2b5\ub2c8\ub2e4./,groups:["value","key"]},{regexp:/No se puede obtener la propiedad '([^']+)' de referencia nula o sin definir/,
groups:["key"]},{regexp:/Unable to get property '([^']+)' of (undefined or null) reference/,groups:["key","value"]}],Error:[{regexp:/(Permission denied) to access property "([^']+)"/,groups:["reason","key"]}]}},{W:function(a){return"Cannot call '"+a.key+"'"},
R:{TypeError:[{regexp:/(?:([^ ]+)?\.)?([^ ]+) is not a function/,groups:["base","key"]},{regexp:/Object (.*) has no method '([^ ]+)'/,groups:["base","key"]},{regexp:/Object doesn't support property or method '([^ ]+)'/,groups:["key"]},{regexp:/\u30aa\u30d6\u30b8\u30a7\u30af\u30c8\u306f '([^']+)' \u30d7\u30ed\u30d1\u30c6\u30a3\u307e\u305f\u306f\u30e1\u30bd\u30c3\u30c9\u3092\u30b5\u30dd\u30fc\u30c8\u3057\u3066\u3044\u307e\u305b\u3093/,groups:["key"]},{regexp:/\uac1c\uccb4\uac00 '([^']+)' \uc18d\uc131\uc774\ub098 \uba54\uc11c\ub4dc\ub97c \uc9c0\uc6d0\ud558\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4./,
groups:["key"]}]}}];function ci(){this.f=[];this.g=[]}
var di;var ei=new O;var fi=new Set,gi=0,hi=["PhantomJS","Googlebot","TO STOP THIS SECURITY SCAN go/scan"];function ii(a,b,c,d){c+="."+a;a=String(JSON.stringify(b)).substr(0,500);d[c]=a;return c.length+a.length}
;function Y(a,b,c){this.l=this.f=this.g=null;this.j=Ua(this);this.h=0;this.s=!1;this.m=[];this.i=null;this.v=c;this.G={};c=document;if(a="string"===typeof a?c.getElementById(a):a)if(c="iframe"==a.tagName.toLowerCase(),b.host||(b.host=c?rc(a.src):"https://www.youtube.com"),this.g=new Re(b),c||(b=ji(this,a),this.l=a,(c=a.parentNode)&&c.replaceChild(b,a),a=b),this.f=a,this.f.id||(this.f.id="widget"+Ua(this.f)),Le[this.f.id]=this,window.postMessage){this.i=new O;ki(this);b=S(this.g,"events");for(var d in b)b.hasOwnProperty(d)&&
this.addEventListener(d,b[d]);for(var e in Ne)li(this,e)}}
p=Y.prototype;p.setSize=function(a,b){this.f.width=a;this.f.height=b;return this};
p.na=function(){return this.f};
p.ba=function(a){this.K(a.event,a)};
p.addEventListener=function(a,b){var c=b;"string"==typeof b&&(c=function(){window[b].apply(window,arguments)});
if(!c)return this;this.i.subscribe(a,c);mi(this,a);return this};
function li(a,b){var c=b.split(".");if(2==c.length){var d=c[1];a.v==c[0]&&mi(a,d)}}
p.destroy=function(){this.f.id&&(Le[this.f.id]=null);var a=this.i;a&&"function"==typeof a.dispose&&a.dispose();if(this.l){a=this.f;var b=a.parentNode;b&&b.replaceChild(this.l,a)}else(a=this.f)&&a.parentNode&&a.parentNode.removeChild(a);Te&&(Te[this.j]=null);this.g=null;a=this.f;for(var c in pb)pb[c][0]==a&&Be(c);this.l=this.f=null};
p.M=function(){return{}};
function ni(a,b,c){c=c||[];c=Array.prototype.slice.call(c);b={event:"command",func:b,args:c};a.s?a.S(b):a.m.push(b)}
p.K=function(a,b){if(!this.i.h){var c={target:this,data:b};this.i.J(a,c);Qe(this.v+"."+a,c)}};
function ji(a,b){for(var c=document.createElement("iframe"),d=b.attributes,e=0,f=d.length;e<f;e++){var g=d[e].value;null!=g&&""!=g&&"null"!=g&&c.setAttribute(d[e].name,g)}c.setAttribute("frameBorder",0);c.setAttribute("allowfullscreen",1);c.setAttribute("allow","accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture");c.setAttribute("title","YouTube "+S(a.g,"title"));(d=S(a.g,"width"))&&c.setAttribute("width",d);(d=S(a.g,"height"))&&c.setAttribute("height",d);var h=a.M();h.enablejsapi=
window.postMessage?1:0;window.location.host&&(h.origin=window.location.protocol+"//"+window.location.host);h.widgetid=a.j;window.location.href&&H(["debugjs","debugcss"],function(k){var l=window.location.href;var m=l.search(uc);b:{var n=0;for(var r=k.length;0<=(n=l.indexOf(k,n))&&n<m;){var q=l.charCodeAt(n-1);if(38==q||63==q)if(q=l.charCodeAt(n+r),!q||61==q||38==q||35==q)break b;n+=r+1}n=-1}if(0>n)l=null;else{r=l.indexOf("&",n);if(0>r||r>m)r=m;n+=k.length+1;l=decodeURIComponent(l.substr(n,r-n).replace(/\+/g,
" "))}null!==l&&(h[k]=l)});
c.src=S(a.g,"host")+a.N()+"?"+tc(h);return c}
p.Z=function(){this.f&&this.f.contentWindow?this.S({event:"listening"}):window.clearInterval(this.h)};
function ki(a){Se(a.g,a,a.j);a.h=Fe(Za(a.Z,a));De(a.f,"load",Za(function(){window.clearInterval(this.h);this.h=Fe(Za(this.Z,this))},a))}
function mi(a,b){a.G[b]||(a.G[b]=!0,ni(a,"addEventListener",[b]))}
p.S=function(a){a.id=this.j;a.channel="widget";var b=Fd(a),c=this.g,d=rc(this.f.src||"");var e=0==d.indexOf("https:")?[d]:c.f?[d.replace("http:","https:")]:c.h?[d]:[d,d.replace("http:","https:")];if(this.f.contentWindow)for(var f=0;f<e.length;f++)try{this.f.contentWindow.postMessage(b,e[f])}catch(Ia){if(Ia.name&&"SyntaxError"==Ia.name){if(!(Ia.message&&0<Ia.message.indexOf("target origin ''"))){var g=void 0,h=void 0,k=Ia;h=void 0===h?{}:h;h.name=P("INNERTUBE_CONTEXT_CLIENT_NAME",1);h.version=P("INNERTUBE_CONTEXT_CLIENT_VERSION",
void 0);var l=h||{},m="WARNING";m=void 0===m?"ERROR":m;g=void 0===g?!1:g;if(k){if(T("console_log_js_exceptions")){var n=k,r=[];r.push("Name: "+n.name);r.push("Message: "+n.message);n.hasOwnProperty("params")&&r.push("Error Params: "+JSON.stringify(n.params));r.push("File name: "+n.fileName);r.push("Stacktrace: "+n.stack);window.console.log(r.join("\n"),n)}if((window&&window.yterr||g)&&!(5<=gi)&&0!==k.sampleWeight){var q=void 0,v=k,w=l,G=fc(v),Ra=G.message||"Unknown Error",ka=G.name||"UnknownError",
Da=G.lineNumber||"Not available",ti=G.fileName||"Not available",ui=G.stack||v.f||"Not available";if(v.hasOwnProperty("args")&&v.args&&v.args.length)for(var qa=0,Mb=0;Mb<v.args.length;Mb++){var Q=v.args[Mb],Aa="params."+Mb;qa+=Aa.length;if(Q)if(Array.isArray(Q)){for(var vi=w,ad=qa,cb=0;cb<Q.length&&!(Q[cb]&&(ad+=ii(cb,Q[cb],Aa,vi),500<ad));cb++);qa=ad}else if("object"===typeof Q){var Nb=void 0,wi=w,bd=qa;for(Nb in Q)if(Q[Nb]&&(bd+=ii(Nb,Q[Nb],Aa,wi),500<bd))break;qa=bd}else w[Aa]=String(JSON.stringify(Q)).substring(0,
500),qa+=w[Aa].length;else w[Aa]=String(JSON.stringify(Q)).substring(0,500),qa+=w[Aa].length;if(500<=qa)break}else if(v.hasOwnProperty("params")&&v.params){var Ob=v.params;if("object"===typeof v.params){var gf=0;for(q in Ob)if(Ob[q]){var hf="params."+q,jf=String(JSON.stringify(Ob[q])).substr(0,500);w[hf]=jf;gf+=hf.length+jf.length;if(500<gf)break}}else w.params=String(JSON.stringify(Ob)).substr(0,500)}navigator.vendor&&!w.hasOwnProperty("vendor")&&(w.vendor=navigator.vendor);var cd={message:Ra,name:ka,
lineNumber:Da,fileName:ti,stack:ui,params:w},kf=Number(v.columnNumber);isNaN(kf)||(cd.lineNumber=cd.lineNumber+":"+kf);for(var ea,Ba=cd,lf=u(bi),dd=lf.next();!dd.done;dd=lf.next()){var ed=dd.value;if(ed.R[Ba.name])for(var mf=u(ed.R[Ba.name]),fd=mf.next();!fd.done;fd=mf.next()){var nf=fd.value,Pb=Ba.message.match(nf.regexp);if(Pb){Ba.params["error.original"]=Pb[0];for(var gd=nf.groups,of={},Ja=0;Ja<gd.length;Ja++)of[gd[Ja]]=Pb[Ja+1],Ba.params["error."+gd[Ja]]=Pb[Ja+1];Ba.message=ed.W(of);break}}}ea=
Ba;window.yterr&&"function"===typeof window.yterr&&window.yterr(ea);if(!(fi.has(ea.message)||0<=ea.stack.indexOf("/YouTubeCenter.js")||0<=ea.stack.indexOf("/mytube.js"))){ei.J("handleError",ea);if(T("kevlar_gel_error_routing")){var Qb=void 0,pf=m,N=ea;a:{for(var qf=u(hi),hd=qf.next();!hd.done;hd=qf.next()){var rf=I;if(rf&&0<=rf.toLowerCase().indexOf(hd.value.toLowerCase())){var sf=!0;break a}}sf=!1}if(!sf){var db={stackTrace:N.stack};N.fileName&&(db.filename=N.fileName);var fa=N.lineNumber&&N.lineNumber.split?
N.lineNumber.split(":"):[];0!==fa.length&&(1!==fa.length||isNaN(Number(fa[0]))?2!==fa.length||isNaN(Number(fa[0]))||isNaN(Number(fa[1]))||(db.lineNumber=Number(fa[0]),db.columnNumber=Number(fa[1])):db.lineNumber=Number(fa[0]));var xi=N.message,yi=N.name;di||(di=new ci);var tf=di;a:{for(var uf=u(tf.g),id=uf.next();!id.done;id=uf.next()){var vf=id.value;if(N.message&&N.message.match(vf.f)){var jd=vf.weight;break a}}for(var wf=u(tf.f),kd=wf.next();!kd.done;kd=wf.next()){var xf=kd.value;if(xf.f(N)){jd=
xf.weight;break a}}jd=1}var ld={level:"ERROR_LEVEL_UNKNOWN",message:xi,errorClassName:yi,sampleWeight:jd};"ERROR"===pf?ld.level="ERROR_LEVEL_ERROR":"WARNING"===pf&&(ld.level="ERROR_LEVEL_WARNNING");var zi={isObfuscated:!0,browserStackInfo:db},Rb={pageUrl:window.location.href,kvPairs:[]},md=N.params;if(md)for(var yf=u(Object.keys(md)),nd=yf.next();!nd.done;nd=yf.next()){var zf=nd.value;Rb.kvPairs.push({key:"client."+zf,value:String(md[zf])})}var Af=P("SERVER_NAME",void 0),Bf=P("SERVER_VERSION",void 0);
Af&&Bf&&(Rb.kvPairs.push({key:"server.name",value:Af}),Rb.kvPairs.push({key:"server.version",value:Bf}));var Ai={errorMetadata:Rb,stackTrace:zi,logMessage:ld};Qb=void 0===Qb?{}:Qb;var Cf=ai;P("ytLoggingEventsDefaultDisabled",!1)&&ai==ai&&(Cf=null);var Bi=Cf,V=Qb;V=void 0===V?{}:V;var eb={};eb.eventTimeMs=Math.round(V.timestamp||U());eb.clientError=Ai;var Ci=String;if(V.timestamp)var Df=-1;else{var Ef=B("_lact",window);if(null==Ef)var Ff=-1;else{var Di=Ef;Ff=Math.max(C()-Di,0)}Df=Ff}eb.context={lastActivityMs:Ci(Df)};
if(T("log_sequence_info_on_gel_web")&&V.ca){var Ei=eb.context,fb=V.ca;fg[fb]=fb in fg?fg[fb]+1:0;Ei.sequence={index:fg[fb],groupKey:fb};V.Ga&&delete fg[V.ca]}var Fi=eb,Sb=V.Fa,Gf=Bi,od="";if(Sb){var Tb=Sb,pd={};Tb.videoId?pd.videoId=Tb.videoId:Tb.playlistId&&(pd.playlistId=Tb.playlistId);Xf[Sb.token]=pd;od=Sb.token}var qd=Wf.get(od)||[];Wf.set(od,qd);qd.push(Fi);Gf&&(Qf=new Gf);var Gi=Ve("web_logging_max_batch")||100,Hf=U();qd.length>=Gi?Yf():10<=Hf-Tf&&($f(),Tf=Hf);Yf()}}var rd,Ca=ea,gb=Ca.params||
{},ra={oa:{a:"logerror",t:"jserror",type:Ca.name,msg:Ca.message.substr(0,250),line:Ca.lineNumber,level:m,"client.name":gb.name},u:{url:P("PAGE_NAME",window.location.href),file:Ca.fileName},method:"POST"};gb.version&&(ra["client.version"]=gb.version);if(ra.u){Ca.stack&&(ra.u.stack=Ca.stack);for(var If=u(Object.keys(gb)),sd=If.next();!sd.done;sd=If.next()){var Jf=sd.value;ra.u["client."+Jf]=gb[Jf]}if(rd=P("LATEST_ECATCHER_SERVICE_TRACKING_PARAMS",void 0))for(var Kf=u(Object.keys(rd)),td=Kf.next();!td.done;td=
Kf.next()){var Lf=td.value;ra.u[Lf]=rd[Lf]}var Mf=P("SERVER_NAME",void 0),Nf=P("SERVER_VERSION",void 0);Mf&&Nf&&(ra.u["server.name"]=Mf,ra.u["server.version"]=Nf)}ug(P("ECATCHER_REPORT_HOST","")+"/error_204",ra);fi.add(ea.message);gi++}}}}}else throw Ia;}else console&&console.warn&&console.warn("The YouTube player is not attached to the DOM. API calls should be made after the onReady event. See more: https://developers.google.com/youtube/iframe_api_reference#Events")};function oi(a){return(0===a.search("cue")||0===a.search("load"))&&"loadModule"!==a}
function pi(a){return 0===a.search("get")||0===a.search("is")}
;function Z(a,b){if(!a)throw Error("YouTube player element ID required.");var c={title:"video player",videoId:"",width:640,height:360};if(b)for(var d in b)c[d]=b[d];Y.call(this,a,c,"player");this.C={};this.playerInfo={}}
sa(Z,Y);p=Z.prototype;p.N=function(){return"/embed/"+S(this.g,"videoId")};
p.M=function(){var a=S(this.g,"playerVars");if(a){var b={},c;for(c in a)b[c]=a[c];a=b}else a={};window!=window.top&&document.referrer&&(a.widget_referrer=document.referrer.substring(0,256));if(c=S(this.g,"embedConfig")){if(Ta(c))try{c=JSON.stringify(c)}catch(d){console.error("Invalid embed config JSON",d)}a.embed_config=c}return a};
p.ba=function(a){var b=a.event;a=a.info;switch(b){case "apiInfoDelivery":if(Ta(a))for(var c in a)this.C[c]=a[c];break;case "infoDelivery":qi(this,a);break;case "initialDelivery":window.clearInterval(this.h);this.playerInfo={};this.C={};ri(this,a.apiInterface);qi(this,a);break;default:this.K(b,a)}};
function qi(a,b){if(Ta(b))for(var c in b)a.playerInfo[c]=b[c]}
function ri(a,b){H(b,function(c){this[c]||("getCurrentTime"==c?this[c]=function(){var d=this.playerInfo.currentTime;if(1==this.playerInfo.playerState){var e=(C()/1E3-this.playerInfo.currentTimeLastUpdated_)*this.playerInfo.playbackRate;0<e&&(d+=Math.min(e,1))}return d}:oi(c)?this[c]=function(){this.playerInfo={};
this.C={};ni(this,c,arguments);return this}:pi(c)?this[c]=function(){var d=0;
0===c.search("get")?d=3:0===c.search("is")&&(d=2);return this.playerInfo[c.charAt(d).toLowerCase()+c.substr(d+1)]}:this[c]=function(){ni(this,c,arguments);
return this})},a)}
p.getVideoEmbedCode=function(){var a=parseInt(S(this.g,"width"),10),b=parseInt(S(this.g,"height"),10),c=S(this.g,"host")+this.N();Cb.test(c)&&(-1!=c.indexOf("&")&&(c=c.replace(wb,"&amp;")),-1!=c.indexOf("<")&&(c=c.replace(xb,"&lt;")),-1!=c.indexOf(">")&&(c=c.replace(yb,"&gt;")),-1!=c.indexOf('"')&&(c=c.replace(zb,"&quot;")),-1!=c.indexOf("'")&&(c=c.replace(Ab,"&#39;")),-1!=c.indexOf("\x00")&&(c=c.replace(Bb,"&#0;")));return'<iframe width="'+a+'" height="'+b+'" src="'+c+'" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'};
p.getOptions=function(a){return this.C.namespaces?a?this.C[a].options||[]:this.C.namespaces||[]:[]};
p.getOption=function(a,b){if(this.C.namespaces&&a&&b)return this.C[a][b]};
function si(a){if("iframe"!=a.tagName.toLowerCase()){var b=R(a,"videoid");b&&(b={videoId:b,width:R(a,"width"),height:R(a,"height")},new Z(a,b))}}
;function Hi(a,b){var c={title:"Thumbnail",videoId:"",width:120,height:68};if(b)for(var d in b)c[d]=b[d];Y.call(this,a,c,"thumbnail")}
sa(Hi,Y);Hi.prototype.N=function(){return"/embed/"+S(this.g,"videoId")};
Hi.prototype.M=function(){return{player:0,thumb_width:S(this.g,"thumbWidth"),thumb_height:S(this.g,"thumbHeight"),thumb_align:S(this.g,"thumbAlign")}};
Hi.prototype.K=function(a,b){Y.prototype.K.call(this,a,b?b.info:void 0)};
function Ii(a){if("iframe"!=a.tagName.toLowerCase()){var b=R(a,"videoid");if(b){b={videoId:b,events:{},width:R(a,"width"),height:R(a,"height"),thumbWidth:R(a,"thumb-width"),thumbHeight:R(a,"thumb-height"),thumbAlign:R(a,"thumb-align")};var c=R(a,"onclick");c&&(b.events.onClick=c);new Hi(a,b)}}}
;D("YT.PlayerState.UNSTARTED",-1);D("YT.PlayerState.ENDED",0);D("YT.PlayerState.PLAYING",1);D("YT.PlayerState.PAUSED",2);D("YT.PlayerState.BUFFERING",3);D("YT.PlayerState.CUED",5);D("YT.get",function(a){return Le[a]});
D("YT.scan",Oe);D("YT.subscribe",function(a,b,c){Zd.subscribe(a,b,c);Ne[a]=!0;for(var d in Le)li(Le[d],a)});
D("YT.unsubscribe",function(a,b,c){Yd(a,b,c)});
D("YT.Player",Z);D("YT.Thumbnail",Hi);Y.prototype.destroy=Y.prototype.destroy;Y.prototype.setSize=Y.prototype.setSize;Y.prototype.getIframe=Y.prototype.na;Y.prototype.addEventListener=Y.prototype.addEventListener;Z.prototype.getVideoEmbedCode=Z.prototype.getVideoEmbedCode;Z.prototype.getOptions=Z.prototype.getOptions;Z.prototype.getOption=Z.prototype.getOption;Me.push(function(a){a=Pe("player",a);H(a,si)});
Me.push(function(){var a=Pe("thumbnail");H(a,Ii)});
"undefined"!=typeof YTConfig&&YTConfig.parsetags&&"onload"!=YTConfig.parsetags||Oe();var Ji=A.onYTReady;Ji&&Ji();var Ki=A.onYouTubeIframeAPIReady;Ki&&Ki();var Li=A.onYouTubePlayerAPIReady;Li&&Li();}).call(this);
