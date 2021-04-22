CREATE TABLE team_font_variant
  id uuid PRIMARY KEY DEFAULT uuid_generate_v4(),

  team_id    uuid NOT NULL REFERENCES team(id) ON DELETE CASCADE,
  profile_id uuid NULL REFERENCES profile(id) ON DELETE SET NULL,

  created_at timestamptz NOT NULL DEFAULT now(),
  modified_at timestamptz NOT NULL DEFAULT now(),

  font_id text NOT NULL,
  font_weight smallint NOT NULL,
  font_style text NOT NULL,

  ttf_file_id   uuid NULL REFERENCES storage_object(id) ON DELETE SET NULL,
  woff1_file_id uuid NULL REFERENCES storage_object(id) ON DELETE SET NULL,
  woff2_file_id uuid NULL REFERENCES storage_object(id) ON DELETE SET NULL,
);
