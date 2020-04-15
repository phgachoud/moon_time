note
	description: "[
		/* +++Date last modified: 05-Jul-1997 */
		/* Updated comments, 05-Aug-2013 */

		/*

		SUNRISET.C - computes Sun rise/set times, start/end of twilight, and
		             the length of the day at any date and latitude

		Written as DAYLEN.C, 1989-08-16

		Modified to SUNRISET.C, 1992-12-01

		(c) Paul Schlyter, 1989, 1992

		Released to the public domain by Paul Schlyter, December 1992

		*/
		]"
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	SUNRISET

inherit
	MATH_CONST

	DOUBLE_MATH

feature -- helpers

	days_since_2000_Jan_0 (y, m, d: REAL_64): INTEGER
			-- Compute days from year-month-day to January 1 2000
		require
			valid_date: attached (create {DATE}.make (y.truncated_to_integer, m.truncated_to_integer, d.truncated_to_integer)) as al_date
			year_range: al_date.year >= 1801 and then al_date.year <= 2099
			match_back: al_date.year.to_real = y and then al_date.month.to_real = m and then al_date.day.to_real = d
		do
			Result := (367 * (y) - ((7 * ((y) + (((m) + 9) / 12))) / 4) + ((275 * (m)) / 9) + (d) - 730_530).truncated_to_integer
		end

feature -- Operations

	sun_rise_set (year,month,day,lon,lat,rise,set: DOUBLE): TUPLE [return_value: INTEGER_32; trise: REAL_64; tset: REAL_64]
			--/* This macro computes times for sunrise/sunset.                      */
			--/* Sunrise/set is considered to occur when the Sun's upper limb is    */
			--/* 35 arc minutes below the horizon (this accounts for the refraction */
			--/* of the Earth's atmosphere).                                        */
		do
			Result := sunriset ( year.truncated_to_integer, month.truncated_to_integer, day.truncated_to_integer, lon, lat, -35.0/60.0, 1 )
		end

	civil_twilight (year,month,day,lon,lat: DOUBLE): TUPLE [return_value: INTEGER_32; trise: REAL_64; tset: REAL_64]
			--/* This macro computes the start and end times of civil twilight.       */
			--/* Civil twilight starts/ends when the Sun's center is 6 degrees below  */
			--/* the horizon.                                                         */
        do
        	Result := sunriset ( year.truncated_to_integer, month.truncated_to_integer, day.truncated_to_integer, lon, lat, -6.0, 0 )
        end

	nautical_twilight (year,month,day,lon,lat: DOUBLE): TUPLE [return_value: INTEGER_32; trise: REAL_64; tset: REAL_64]
			--/* This macro computes the start and end times of nautical twilight.    */
			--/* Nautical twilight starts/ends when the Sun's center is 12 degrees    */
			--/* below the horizon.                                                   */
        do
        	Result := sunriset ( year.truncated_to_integer, month.truncated_to_integer, day.truncated_to_integer, lon, lat, -12.0, 0 )
        end

	astronomical_twilight (year,month,day,lon,lat: DOUBLE): TUPLE [return_value: INTEGER_32; trise: REAL_64; tset: REAL_64]
			--/* This macro computes the start and end times of astronomical twilight.   */
			--/* Astronomical twilight starts/ends when the Sun's center is 18 degrees   */
			--/* below the horizon.                                                      */
        do
        	Result := sunriset ( year.truncated_to_integer, month.truncated_to_integer, day.truncated_to_integer, lon, lat, -18.0, 0 )
        end

feature {NONE} -- Macros

--/* Following are some macros around the "workhorse" function __daylen__ */
--/* They mainly fill in the desired values for the reference altitude    */
--/* below the horizon, and also selects whether this altitude should     */
--/* refer to the Sun's center or its upper limb.                         */

	day_length (year,month,day,lon,lat: DOUBLE): DOUBLE
			--/* This macro computes the length of the day, from sunrise to sunset. */
			--/* Sunrise/set is considered to occur when the Sun's upper limb is    */
			--/* 35 arc minutes below the horizon (this accounts for the refraction */
			--/* of the Earth's atmosphere).                                        */
		do
			Result := daylen ( year.truncated_to_integer, month.truncated_to_integer, day.truncated_to_integer, lon, lat, -35.0/60.0, 1 )
		end

	day_civil_twilight_length (year,month,day,lon,lat: DOUBLE): DOUBLE
			--/* This macro computes the length of the day, including civil twilight. */
			--/* Civil twilight starts/ends when the Sun's center is 6 degrees below  */
			--/* the horizon.                                                         */
		do
			Result := daylen ( year.truncated_to_integer, month.truncated_to_integer, day.truncated_to_integer, lon, lat, -6.0, 0 )
		end

	day_nautical_twilight_length (year,month,day,lon,lat: DOUBLE): DOUBLE
			--/* This macro computes the length of the day, incl. nautical twilight.  */
			--/* Nautical twilight starts/ends when the Sun's center is 12 degrees    */
			--/* below the horizon.                                                   */
        do
        	Result := daylen ( year.truncated_to_integer, month.truncated_to_integer, day.truncated_to_integer, lon, lat, -12.0, 0 )
        end

	day_astronomical_twilight_length (year,month,day,lon,lat: DOUBLE): DOUBLE
			--/* This macro computes the length of the day, incl. astronomical twilight. */
			--/* Astronomical twilight starts/ends when the Sun's center is 18 degrees   */
			--/* below the horizon.                                                      */
        do
        	Result := daylen ( year.truncated_to_integer, month.truncated_to_integer, day.truncated_to_integer, lon, lat, -18.0, 0 )
        end

feature {NONE} -- daylen

	daylen (year, month, day: INTEGER; lon, lat, altit: DOUBLE; upper_limb: INTEGER): DOUBLE
			-- /* The "workhorse" function */
		note
			description: "[
				/**********************************************************************/
				/* Note: year,month,date = calendar date, 1801-2099 only.             */
				/*       Eastern longitude positive, Western longitude negative       */
				/*       Northern latitude positive, Southern latitude negative       */
				/*       The longitude value is not critical. Set it to the correct   */
				/*       longitude if you're picky, otherwise set to to, say, 0.0     */
				/*       The latitude however IS critical - be sure to get it correct */
				/*       altit = the altitude which the Sun should cross              */
				/*               Set to -35/60 degrees for rise/set, -6 degrees       */
				/*               for civil, -12 degrees for nautical and -18          */
				/*               degrees for astronomical twilight.                   */
				/*         upper_limb: non-zero -> upper limb, zero -> center         */
				/*               Set to non-zero (e.g. 1) when computing day length   */
				/*               and to zero when computing day+twilight length.      */
				/**********************************************************************/
				]"
		require
			year_range: (year >= 1801 and then year <= 2099)
			limb_boolean: (upper_limb = 0 or else upper_limb = 1) -- See note above
		local
			d: DOUBLE 			--,  /* Days since 2000 Jan 0.0 (negative before) */
			obl_ecl: DOUBLE 	--,  /* Obliquity (inclination) of Earth's axis */
			sr: DOUBLE 			--,  /* Solar distance, astronomical units */
			slon: DOUBLE 		--,  /* True solar longitude */
			sin_sdecl: DOUBLE 	--,  /* Sine of Sun's declination */
			cos_sdecl: DOUBLE 	--,  /* Cosine of Sun's declination */
			sradius: DOUBLE 	--,  /* Sun's apparent radius */
			t: DOUBLE 			--;  /* Diurnal arc */
			l_sunpos: like sunpos
			cost: DOUBLE
		do
			-- /* Compute d of 12h local mean solar time */
			d := days_since_2000_Jan_0 (year,month,day) + 0.5 - lon/360.0;

			-- /* Compute obliquity of ecliptic (inclination of Earth's axis) */
			obl_ecl := 23.4393 - 3.563E-7 * d;

			-- /* Compute Sun's ecliptic longitude and distance */
			l_sunpos := sunpos(d);

			-- /* Compute sine and cosine of Sun's declination */
			sin_sdecl := sind(obl_ecl) * sind(slon);
			cos_sdecl := sqrt( 1.0 - sin_sdecl * sin_sdecl );

			-- /* Compute the Sun's apparent radius, degrees */
			sradius := 0.2666 / sr;

			-- /* Do correction to upper limb, if necessary */
			if ( upper_limb = 1 ) then
				-- /* Compute the diurnal arc that the Sun traverses to reach */
				-- /* the specified altitude altit: */
				cost := ( sind (altit - sradius) - sind (lat) * sin_sdecl ) / ( cosd(lat) * cos_sdecl );
				if ( cost >= 1.0 ) then
					t := 0.0;                      		-- /* Sun always below altit */
				elseif ( cost <= -1.0 ) then
	                  t := 24.0;                     	-- /* Sun always above altit */
				else  t := (2.0/15.0) * acosd (cost); 	-- /* The diurnal arc, hours */
				end
			end
			Result := t;
		end

feature {NONE} -- sunriset

	sunriset (year, month, day: INTEGER; lon, lat, altit: DOUBLE; upper_limb: INTEGER): TUPLE [return_value: INTEGER; trise, tset: DOUBLE]
			-- The "workhorse" function for sun rise/set times
			-- ( int year, int month, int day, double lon, double lat,
			--      double altit, int upper_limb, double *trise, double *tset )
		note
			description: "[
				/***************************************************************************/
				/* Note: year,month,date = calendar date, 1801-2099 only.             */
				/*       Eastern longitude positive, Western longitude negative       */
				/*       Northern latitude positive, Southern latitude negative       */
				/*       The longitude value IS critical in this function!            */
				/*       altit = the altitude which the Sun should cross              */
				/*               Set to -35/60 degrees for rise/set, -6 degrees       */
				/*               for civil, -12 degrees for nautical and -18          */
				/*               degrees for astronomical twilight.                   */
				/*         upper_limb: non-zero -> upper limb, zero -> center         */
				/*               Set to non-zero (e.g. 1) when computing rise/set     */
				/*               times, and to zero when computing start/end of       */
				/*               twilight.                                            */
				/*        *rise = where to store the rise time                        */
				/*        *set  = where to store the set  time                        */
				/*                Both times are relative to the specified altitude,  */
				/*                and thus this function can be used to compute       */
				/*                various twilight times, as well as rise/set times   */
				/* Return value:  0 = sun rises/sets this day, times stored at        */
				/*                    *trise and *tset.                               */
				/*               +1 = sun above the specified "horizon" 24 hours.     */
				/*                    *trise set to time when the sun is at south,    */
				/*                    minus 12 hours while *tset is set to the south  */
				/*                    time plus 12 hours. "Day" length = 24 hours     */
				/*               -1 = sun is below the specified "horizon" 24 hours   */
				/*                    "Day" length = 0 hours, *trise and *tset are    */
				/*                    both set to the time when the sun is at south.  */
				/*                                                                    */
				/**********************************************************************/
				]"
		require
			valid_date: attached (create {DATE}.make (year, month, day)) as al_date
			year_range: al_date.year >= 1801 and then al_date.year <= 2099
			match_back: al_date.year = year and then al_date.month = month and then al_date.day = day
			lon_range: lon >= -180 and then lon <= 180
			lat_range: lat >= -90 and then lat <= 90
		local
			d: DOUBLE 		--,  /* Days since 2000 Jan 0.0 (negative before) */
			sr: DOUBLE 		--,  /* Solar distance, astronomical units */
			sRA: DOUBLE 	--,  /* Sun's Right Ascension */
			sdec: DOUBLE 	--,  /* Sun's declination */
			sradius: DOUBLE --,  /* Sun's apparent radius */
			t: DOUBLE 		--,  /* Diurnal arc */
			tsouth: DOUBLE 	--,  /* Time when Sun is at south */
			sidtime: DOUBLE --;  /* Local sidereal time */
			l_sun_RA_dec: like sun_ra_dec
			rc: INTEGER
			cost: DOUBLE
		do
			create Result
			--int rc := 0; -- /* Return cde from function - usually 0 */

			-- /* Compute d of 12h local mean solar time */
			d := days_since_2000_Jan_0(year,month,day) + 0.5 - lon/360.0;

			-- /* Compute the local sidereal time of this moment */
			sidtime := revolution( GMST0(d) + 180.0 + lon );

			-- /* Compute Sun's RA, Decl and distance at this moment */
			l_sun_RA_dec := sun_RA_dec( d, 0)--, &sRA, &sdec, &sr );

			-- /* Compute time when Sun is at south - in hours UT */
			tsouth := 12.0 - rev180(sidtime - l_sun_RA_dec.ra)/15.0;

			-- /* Compute the Sun's apparent radius in degrees */
			if l_sun_RA_dec.sradius = 0.0 then
				sradius := 0.0
			else
				sradius := 0.2666 / l_sun_RA_dec.sradius;
			end

			-- /* Do correction to upper limb, if necessary */
			if ( upper_limb = 1 ) then
				-- altit -= sradius;

				-- /* Compute the diurnal arc that the Sun traverses to reach */
				-- /* the specified altitude altit: */
				--{
					--double cost;
					cost := ( sind(altit - sradius) - sind(lat) * sind(sdec) ) / ( cosd(lat) * cosd(sdec) );
					if ( cost >= 1.0 ) then
						rc := -1; t := 0.0;       -- /* Sun always below altit */
					elseif ( cost <= -1.0 ) then
						rc := +1; t := 12.0;      -- /* Sun always above altit */
					else
						t := acosd(cost)/15.0;   -- /* The diurnal arc, hours */
					end
				--}
			end
			-- /* Store rise and set times - in hours UT */
			Result.trise := tsouth - t;
			Result.tset  := tsouth + t;
			Result.return_value := rc
			-- return rc;
		end

feature {NONE} -- sunpos

	sunpos (a_number_of_days: DOUBLE): TUPLE [longitude, radius_vector: DOUBLE]
			-- `a_number_of_days' since 2000 Jan 0.0
		note
			description: "[
				/******************************************************/
				/* Computes the Sun's ecliptic longitude and distance */
				/* at an instant given in d, `a_number_of_days' since     */
				/* 2000 Jan 0.0.  The Sun's ecliptic latitude is not  */
				/* computed, since it's always very near 0.           */
				/******************************************************/
				]"
			glossary: "[
				Perihelion - the point in the orbit of a planet, asteroid, 
								or comet at which it is closest to the sun.
				Eccentricity - deviation of a curve or orbit from circularity.
				]"
			EIS: "name=perihelion", "src=https://simple.wikipedia.org/wiki/Perihelion"
		local
			l_mean_anomaly: DOUBLE --,		/* Mean (M) anomaly of the Sun */
			w: DOUBLE --,         			/* Mean longitude of perihelion */
						--		  			/* Note: Sun's mean longitude = M + w */
			ee: DOUBLE --,         			/* Eccentricity of Earth's orbit */
			Ea: DOUBLE --,         			/* Eccentric anomaly */
			x, y: DOUBLE --,      			/* x, y coordinates in orbit */
			v: DOUBLE --;         			/* True anomaly */
		do
			create Result
			-- /* Compute mean elements */
			l_mean_anomaly := revolution ( 356.0470 + 0.9856002585 * a_number_of_days );
			w := 282.9404 + 4.70935E-5 * a_number_of_days;
			ee := 0.016709 - 1.151E-9 * a_number_of_days;

			-- /* Compute true longitude and radius vector */
			Ea := l_mean_anomaly + ee * RADEG * sind (l_mean_anomaly) * ( 1.0 + ee * cosd (l_mean_anomaly) );
			x := cosd(Ea) - ee;
			y := sqrt( 1.0 - ee*ee ) * sind (Ea);
			Result.radius_vector := sqrt( x*x + y*y );         -- /* Solar distance */
			v := atan2d( y, x );                  		-- /* True anomaly */
			Result.longitude := v + w;                        	-- /* True solar longitude */
			if ( Result.longitude >= 360.0 ) then
				Result.longitude := Result.longitude - 360.0;         -- /* Make it 0..360 degrees */
			end
		end

	sun_RA_dec (a_number_of_days, a_r: DOUBLE): TUPLE [RA, dec, sradius: DOUBLE]
			-- sun_RA_dec( double d, double *RA, double *dec, double *r )
		note
			description: "[
				/******************************************************/
				/* Computes the Sun's equatorial coordinates RA, Decl */
				/* and also its distance, at an instant given in d,   */
				/* the number of days since 2000 Jan 0.0.             */
				/******************************************************/
				]"
		local
			lon, obl_ecl, x, y, z: DOUBLE
			l_sunpos: like sunpos
		do
			create Result

			-- /* Compute Sun's ecliptical coordinates */
			l_sunpos := sunpos (a_number_of_days); -- yields lon and r

			-- /* Compute ecliptic rectangular coordinates (z=0) */
			x := l_sunpos.radius_vector * cosd (lon);
			y := l_sunpos.radius_vector * sind (lon);

			-- /* Compute obliquity of ecliptic (inclination of Earth's axis) */
			obl_ecl := 23.4393 - 3.563E-7 * a_number_of_days;

			-- /* Convert to equatorial rectangular coordinates - x is unchanged */
			z := y * sind (obl_ecl);
			y := y * cosd (obl_ecl);

			-- /* Convert to spherical coordinates */
			Result.RA := atan2d( y, x );
			Result.dec := atan2d( z, sqrt(x*x + y*y) );
			Result.sradius := a_r
		end

feature {NONE} -- Functions

-- /* Some conversion factors between radians and degrees */

	RADEG: DOUBLE once Result := ( 180.0 / pi ) end
			-- Radians -> Degrees

	DEGRAD: DOUBLE once Result := (pi / 180) end
			-- Degrees -> Radians

-- /* The trigonometric functions in degrees */

	sind (x: DOUBLE): DOUBLE do Result := sin((x) * DEGRAD) end
			-- Sine of `x'

	sin (x: DOUBLE): DOUBLE
			-- External `sin' (sine)
		external
			"[
				C signature (double): double use <math.h>
				]"
		end

	cosd (x: DOUBLE): DOUBLE do Result := cos ((x) * DEGRAD) end
			-- Cosine of `x'

	cos (x: DOUBLE): DOUBLE
			-- External `cos' (cosine)
		external
			"[
				C signature (double): double use <math.h>
				]"
		end

	tand (x: DOUBLE): DOUBLE do Result := tan((x) * DEGRAD) end
			-- Tangent of `x'

	tan (x: DOUBLE): DOUBLE
			-- External `tan' (tangent)
		external
			"[
				C signature (double): double use <math.h>
				]"
		end

	atand (x: DOUBLE): DOUBLE do Result := RADEG * atan(x) end
			--#define atand(x)    (RADEG*atan(x))

	atan (x: DOUBLE): DOUBLE
			-- External `atan' (arc-tangent)
		external
			"[
				C signature (double): double use <math.h>
				]"
		end

	asind (x: DOUBLE): DOUBLE do Result := RADEG * asin(x) end
			--#define asind(x)    (RADEG*asin(x))

	asin (x: DOUBLE): DOUBLE
			-- External `asin' (arc-sine)
		external
			"[
				C signature (double): double use <math.h>
				]"
		end

	acosd (x: DOUBLE): DOUBLE do Result := RADEG * acos(x) end
			--#define acosd(x)    (RADEG*acos(x))

	acos (x: DOUBLE): DOUBLE
			-- External `acos' (arc-cosine)
		external
			"[
				C signature (double): double use <math.h>
				]"
		end

	atan2d (x, y: DOUBLE): DOUBLE do Result := RADEG * atan2(y,x) end
			-- 2D Arc-tangent Radian Degrees

	atan2 (y, x: DOUBLE): DOUBLE
			-- 2d arc tangent of (y, x)
		note
			description: "The 2 dimensional arc-tangent of x/y"
		external
			"[
				C signature (double, double): double use <math.h>
				]"
		end

feature {NONE} -- revolution

	revolution (x: DOUBLE): DOUBLE
			-- Reduce angle to within 0..360 degrees
		note
			description: "[
				/******************************************************************/
				/* This function reduces any angle to within the first revolution */
				/* by subtracting or adding even multiples of 360.0 until the     */
				/* result is >= 0.0 and < 360.0                                   */
				/******************************************************************/
				]"
		do
			Result := (x - 360.0 * ( x * inv360 ).floor)
		end

	inv360: DOUBLE once Result := ( 1.0 / 360.0 ) end

	rev180 (x: DOUBLE): DOUBLE
			-- Reduce angle to within +180..+180 degrees
		do
			Result := (x - 360.0 * ( x * inv360 + 0.5 ).floor)
		end

	GMST0 (d: DOUBLE): DOUBLE
			--
		note
			description: "[
				/*******************************************************************/
				/* This function computes GMST0, the Greenwich Mean Sidereal Time  */
				/* at 0h UT (i.e. the sidereal time at the Greenwhich meridian at  */
				/* 0h UT).  GMST is then the sidereal time at Greenwich at any     */
				/* time of the day.  I've generalized GMST0 as well, and define it */
				/* as:  GMST0 = GMST - UT  --  this allows GMST0 to be computed at */
				/* other times than 0h UT as well.  While this sounds somewhat     */
				/* contradictory, it is very practical:  instead of computing      */
				/* GMST like:                                                      */
				/*                                                                 */
				/*  GMST = (GMST0) + UT * (366.2422/365.2422)                      */
				/*                                                                 */
				/* where (GMST0) is the GMST last time UT was 0 hours, one simply  */
				/* computes:                                                       */
				/*                                                                 */
				/*  GMST = GMST0 + UT                                              */
				/*                                                                 */
				/* where GMST0 is the GMST "at 0h UT" but at the current moment!   */
				/* Defined in this way, GMST0 will increase with about 4 min a     */
				/* day.  It also happens that GMST0 (in degrees, 1 hr = 15 degr)   */
				/* is equal to the Sun's mean longitude plus/minus 180 degrees!    */
				/* (if we neglect aberration, which amounts to 20 seconds of arc   */
				/* or 1.33 seconds of time)                                        */
				/*                                                                 */
				/*******************************************************************/
				]"
		local
			sidtim0: DOUBLE
		do
	--      /* Sidtime at 0h UT = L (Sun's mean longitude) + 180.0 degr  */
	--      /* L = M + w, as defined in sunpos().  Since I'm too lazy to */
	--      /* add these numbers, I'll let the C compiler do it for me.  */
	--      /* Any decent C compiler will add the constants at compile   */
	--      /* time, imposing no runtime or code overhead.               */
			sidtim0 := revolution( ( 180.0 + 356.0470 + 282.9404 ) + ( 0.9856002585 + 4.70935E-5 ) * d )
			Result := sidtim0
		end

end
