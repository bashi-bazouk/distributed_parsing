open RegularExpressions
open List

type regexp = Enhanced.t

type cursor = {
	regexp: regexp;
	start_at: int;
	subbuffering: int;
}

let should_buffer c = c.subbuffering > 0

