
SELECT 
	gl.slug_season,
	nm.display_first_last AS player_name,
	gl.game_id,
	gl.min,
	gl.fgm,
	gl.fga,
	gl.fg3_m,
	gl.fg3_a,
	gl.ftm,
	gl.fta,
	gl.pts,
	gl.pf,
	gl.reb,
	gl.ast,
	gl.stl,
	gl.blk,
	gl.tov	
	
FROM nba.player_game_log AS gl
LEFT JOIN nba.player_info AS nm ON gl.player_id = nm.person_id 

