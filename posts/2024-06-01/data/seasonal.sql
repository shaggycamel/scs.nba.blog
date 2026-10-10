
SELECT 
	st.season_id,
	nm.display_first_last AS player_name,
	st.gp,
	st.gs,
	st.min,
	st.fgm,
	st.fga,
	st.fg3_m,
	st.fg3_a,
	st.ftm,
	st.fta,
	st.pts,
	st.pf,
	st.reb,
	st.ast,
	st.stl,
	st.blk,
	st.tov	
	
FROM nba.player_season_stats AS st
LEFT JOIN nba.player_info AS nm ON st.player_id = nm.person_id 

