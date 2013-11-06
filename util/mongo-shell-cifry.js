// usage:  mongo --shell mongo-shell-cifry.js
db = db.getSiblingDB('zifry');

var products = db.getCollection('PRODUCT');
var groups = db.getCollection('GROUP');
var filters = db.getCollection('FILTER');
var vendors = db.getCollection('VENDOR');
var bonuscards = db.getCollection('ESHOP::BONUSCARD');
var orders = db.getCollection('ESHOP::ORDER');
var sessins = db.getCollection('ESHOP::SESSION');
var users = db.getCollection('ESHOP::USER');
var validations = db.getCollection('ESHOP::VALIDATION');
var paswordResets = db.getCollection('ESHOP::PASSWORD-RESET');

print("<<<<<<<<<<<>>>>>>>>>>>");
print("USE db 'zifry'");
print("collections aliases");
print("products      | " + products.count());
print("groups        | " + groups.count());
print("filter        | " + filters.count());
print("bonuscards    | " + bonuscards.count());
print("orders        | " + orders.count());
print("sessins       | " + sessins.count());
print("users         | " + users.count());
print("validations   | " + validations.count());
print("paswordResets | " + paswordResets.count());
print("<<<<<<<<<<<>>>>>>>>>>>");
