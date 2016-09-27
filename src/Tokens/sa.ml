
(* Helper types*)

type uuid = string

type maybe = unit option

let yes = Some ()
let no = None

(* Locations *)
type user_location = uuid

type location = string list

type china_city = string
type china_country = string
type china_region = string

type city = string
type country = string

type lat = float
type lon = float
type region = string list
type neighborhood = string
type zip = string

type radius = int

(* Pagination *)
type sort_by =
	| RecentlyActive
	| Nearest
	| Newest

type page = int
type limit = int


(* Booleans *)
type photos = maybe
type background_check = maybe
type premium = maybe
type college = maybe
type viewed = maybe
type unviewed = maybe
type viewed_me = maybe
type favorited = maybe
type favorited_me = maybe
type recent_active_only = maybe

(* Non-Booleans *)

type q = string list

type max_age = int option (* years greater than 18, less than 60 *)
type min_age = int option (* years greater than 18, less than 60 *)

type min_height = int option (* centimeters greater than 137, less than 213 *)
type max_height = int option (* centimeters greater than 137, less than 213 *)

(* Miscellanous *)

type diamond = maybe
type female = maybe
type male = maybe