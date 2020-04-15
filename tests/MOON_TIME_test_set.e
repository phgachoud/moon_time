note
	description: "Tests of {MOON_TIME}."
	testing: "type/manual"

class
	MOON_TIME_TEST_SET

inherit
	TEST_SET_SUPPORT

	TEST_SET_BRIDGE
		undefine
			default_create
		end

feature -- Test routines

	MOON_TIME_tests
			-- `MOON_TIME_tests'
		local
			l_item: SUNRISET
		do
			create l_item
			assert_integers_equal ("days_since_2000", 7335, l_item.days_since_2000_Jan_0 (2020, 1, 31))
		end

	sunriset_test
		local
			l_item: SUNRISET
		do
			create l_item
			assert_equal ("30_30", [0, 13.347000499347764, 25.436811058429996], l_item.sun_rise_set (2020, 1, 31, 30, 30, 0, 1))
			assert_equal ("0_0", [0, 15.34754107137776, 27.425318849155538], l_item.sun_rise_set (2020, 1, 31, 0, 0, 0, 1))
			assert_equal ("civil_twilight", [0, 21.38642996026665, 21.38642996026665], l_item.civil_twilight (2020, 1, 31, 0, 0))
		end

end
