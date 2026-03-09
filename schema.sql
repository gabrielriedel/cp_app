create table core_level.trackman_event (
  pitchuid text not null,
  date date not null,
  gameid text not null,
  pitchno integer null,
  first_seen_at timestamp with time zone not null default now(),
  last_updated_at timestamp with time zone not null default now(),
  source_raw_id bigint null,
  time text null,
  utcdate date null,
  utctime time without time zone null,
  localdatetime timestamp with time zone null,
  utcdatetime timestamp with time zone null,
  hometeam text null,
  awayteam text null,
  stadium text null,
  level text null,
  league text null,
  gameuid text null,
  gameforeignid text null,
  hometeamforeignid text null,
  awayteamforeignid text null,
  system text null,
  inning integer null,
  topbottom text null,
  outs integer null,
  balls integer null,
  strikes integer null,
  paofinning text null,
  pitchofpa text null,
  pitcher text null,
  pitcherid text null,
  pitcherteam text null,
  pitcherthrows text null,
  pitcherset text null,
  batter text null,
  batterid text null,
  batterteam text null,
  batterside text null,
  catcher text null,
  catcherid text null,
  catcherteam text null,
  catcherthrows text null,
  taggedpitchtype text null,
  autopitchtype text null,
  pitchcall text null,
  relspeed numeric(6, 2) null,
  spinrate numeric(8, 2) null,
  spinaxis numeric(8, 4) null,
  tilt text null,
  extension numeric(6, 3) null,
  effectivevelo numeric(6, 2) null,
  relheight numeric(6, 3) null,
  relside numeric(6, 3) null,
  vertrelangle numeric(8, 4) null,
  horzrelangle numeric(8, 4) null,
  inducedvertbreak numeric(8, 4) null,
  horzbreak numeric(8, 4) null,
  vertbreak numeric(8, 4) null,
  pfxx numeric(8, 4) null,
  pfxz numeric(8, 4) null,
  platelocheight numeric(6, 3) null,
  platelocside numeric(6, 3) null,
  zonespeed numeric(6, 2) null,
  vertapprangle numeric(8, 4) null,
  horzapprangle numeric(8, 4) null,
  zonetime numeric(8, 5) null,
  x0 numeric(8, 4) null,
  y0 numeric(8, 4) null,
  z0 numeric(8, 4) null,
  vx0 numeric(8, 4) null,
  vy0 numeric(8, 4) null,
  vz0 numeric(8, 4) null,
  ax0 numeric(8, 4) null,
  ay0 numeric(8, 4) null,
  az0 numeric(8, 4) null,
  pitchtrajectoryxc0 numeric(12, 6) null,
  pitchtrajectoryxc1 numeric(12, 6) null,
  pitchtrajectoryxc2 numeric(12, 6) null,
  pitchtrajectoryyc0 numeric(12, 6) null,
  pitchtrajectoryyc1 numeric(12, 6) null,
  pitchtrajectoryyc2 numeric(12, 6) null,
  pitchtrajectoryzc0 numeric(12, 6) null,
  pitchtrajectoryzc1 numeric(12, 6) null,
  pitchtrajectoryzc2 numeric(12, 6) null,
  taggedhittype text null,
  autohittype text null,
  exitspeed numeric(6, 2) null,
  angle numeric(8, 4) null,
  direction numeric(8, 4) null,
  distance numeric(6, 2) null,
  hitspinrate numeric(8, 2) null,
  hitspinaxis numeric(8, 4) null,
  hangtime numeric(6, 3) null,
  bearing numeric(8, 4) null,
  positionat110x numeric(8, 4) null,
  positionat110y numeric(8, 4) null,
  positionat110z numeric(8, 4) null,
  lasttrackeddistance numeric(6, 2) null,
  contactpositionx numeric(8, 4) null,
  contactpositiony numeric(8, 4) null,
  contactpositionz numeric(8, 4) null,
  pitchlastmeasuredx numeric(8, 4) null,
  pitchlastmeasuredy numeric(8, 4) null,
  pitchlastmeasuredz numeric(8, 4) null,
  playresult text null,
  korbb text null,
  outsonplay integer null,
  runsscored integer null,
  notes text null,
  playid text null,
  maxheight numeric(6, 2) null,
  measuredduration numeric(8, 5) null,
  speeddrop numeric(6, 2) null,
  created_at timestamp with time zone null default now(),
  updated_at timestamp with time zone null default now(),
  constraint trackman_event_pkey primary key (pitchuid, date),
  constraint trackman_event_gameid_pitchno_date_key unique (gameid, pitchno, date)
)
partition by
  RANGE (date);

create index IF not exists idx_event_pitcher on only core_level.trackman_event using btree (pitcher);

create index IF not exists idx_event_batter on only core_level.trackman_event using btree (batter);

create index IF not exists idx_event_gameid on only core_level.trackman_event using btree (gameid);

create index IF not exists idx_event_pitchtype on only core_level.trackman_event using btree (taggedpitchtype);

create index IF not exists idx_event_pitchcall on only core_level.trackman_event using btree (pitchcall);

create index IF not exists idx_event_pitcher_date on only core_level.trackman_event using btree (pitcher, date);

create index IF not exists idx_event_batter_date on only core_level.trackman_event using btree (batter, date);

-- Scouting notes table for persistent pitcher notes
CREATE TABLE IF NOT EXISTS scouting_notes (
  id SERIAL PRIMARY KEY,
  pitcher_name TEXT NOT NULL,
  team_name TEXT NOT NULL,
  notes_gameplan TEXT,
  notes_attack TEXT,
  notes_first_pitch TEXT,
  notes_hitter_adv TEXT,
  notes_2k TEXT,
  notes_risp TEXT,
  pitch_descriptions JSONB,
  risp_images JSONB,
  risp_usages JSONB,
  pitcher_stats JSONB,  -- {"ip": "45.2", "era": "3.21", "k": "52", "bb": "12", "baa": ".234"}
  pitcher_grade TEXT,
  out_pitch TEXT,
  velo_overrides JSONB,
  pitch_deletions JSONB,
  pitch_remaps JSONB,
  updated_at TIMESTAMP DEFAULT NOW(),
  UNIQUE(pitcher_name, team_name)
);

-- If table already exists, add the new columns:
-- ALTER TABLE scouting_notes ADD COLUMN IF NOT EXISTS notes_gameplan TEXT;
-- ALTER TABLE scouting_notes ADD COLUMN IF NOT EXISTS pitch_descriptions JSONB;
-- ALTER TABLE scouting_notes ADD COLUMN IF NOT EXISTS risp_images JSONB;
-- ALTER TABLE scouting_notes ADD COLUMN IF NOT EXISTS notes_risp TEXT;
-- ALTER TABLE scouting_notes ADD COLUMN IF NOT EXISTS risp_usages JSONB;
-- ALTER TABLE scouting_notes ADD COLUMN IF NOT EXISTS pitcher_stats JSONB;
-- ALTER TABLE scouting_notes ADD COLUMN IF NOT EXISTS pitcher_grade TEXT;
-- ALTER TABLE scouting_notes ADD COLUMN IF NOT EXISTS out_pitch TEXT;
-- ALTER TABLE scouting_notes ADD COLUMN IF NOT EXISTS velo_overrides JSONB;
-- ALTER TABLE scouting_notes ADD COLUMN IF NOT EXISTS pitch_deletions JSONB;
-- ALTER TABLE scouting_notes ADD COLUMN IF NOT EXISTS pitch_remaps JSONB;

-- Create index for fast lookups
CREATE INDEX IF NOT EXISTS idx_scouting_notes_pitcher
ON scouting_notes(pitcher_name, team_name);

-- Upcoming opponents table for faster scouting data loading
CREATE TABLE IF NOT EXISTS upcoming_opponents (
  id SERIAL PRIMARY KEY,
  team_code TEXT NOT NULL UNIQUE,
  team_name TEXT,
  game_date DATE,
  created_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_upcoming_opponents_team
ON upcoming_opponents(team_code);

-- Add missing indexes for faster team/pitcher queries
CREATE INDEX IF NOT EXISTS idx_event_pitcherteam_batterteam
ON core_level.trackman_event(pitcherteam, batterteam);

CREATE INDEX IF NOT EXISTS idx_event_batterteam
ON core_level.trackman_event(batterteam);

-- Hitter scouting notes table for opposing hitter scouting reports
CREATE TABLE IF NOT EXISTS hitter_scouting_notes (
  id SERIAL PRIMARY KEY,
  batter_name TEXT NOT NULL,
  team_name TEXT NOT NULL,  -- Composite: "TEAM_CODE::RHP" or "TEAM_CODE::LHP"

  -- Count percentages (JSONB for flexibility)
  count_data JSONB,  -- {"0-0": ["15", "20"], "0-1": ["10", "25"], ...}

  -- Highlight states (JSONB)
  highlights JSONB,  -- {"0-2": "yellow", "1-2": "yellow", ...}

  -- Stats text inputs
  stats_slg TEXT,
  stats_run TEXT,
  stats_k TEXT,
  stats_bb TEXT,
  stats_hbp TEXT,
  stats_hr TEXT,
  stats_fly TEXT,
  stats_ground TEXT,

  -- Notes
  notes_main TEXT,
  notes_action TEXT,

  -- Image URLs (stored in Supabase)
  img_box TEXT,
  img_contact_point TEXT,
  img_spray_chart TEXT,
  img_risp_gb TEXT,

  updated_at TIMESTAMP DEFAULT NOW(),
  UNIQUE(batter_name, team_name)
);

CREATE INDEX IF NOT EXISTS idx_hitter_scouting_batter
ON hitter_scouting_notes(batter_name, team_name);