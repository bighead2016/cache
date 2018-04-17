%% 2013-6-13
%% mail:laojiajie@gmail.com
-module(map_data).

-include("cache.hrl").
-include("role.hrl").
-include("account.hrl").
-include("item.hrl").
-include("economy.hrl").


-export([tabs/0, map/1, cache_opt/1,not_init_tab/0,init_sql/0]). 


init_sql() ->
    "set global innodb_flush_log_at_trx_commit=2;"++

    "CREATE TABLE IF NOT EXISTS `gd_timer_http_sender` (
      `msg_id` varchar(64) NOT NULL,
      `key_words` varchar(64) NOT NULL,
      `send_type` varchar(16) NOT NULL,
      `send_url` varchar(512) NOT NULL,
      `send_body` varchar(5120) NOT NULL,
      `send_times` int(11) NOT NULL DEFAULT '0',
      `inter_time` int(11) NOT NULL DEFAULT '0',
      `next_send_time` int(11) NOT NULL DEFAULT '0',
      `err_msg` varchar(64) NOT NULL,
      `call_back` varchar(128) NOT NULL,
      PRIMARY KEY (`msg_id`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8;"++
    
    "CREATE TABLE IF NOT EXISTS `gd_activity_tl_channel_recharge_cfg` (
      `channel` varchar(128) NOT NULL DEFAULT '[]',
      `stage` int(11) NOT NULL DEFAULT '0',
      `min_money` int(11) NOT NULL DEFAULT '0',
      `max_money` int(11) NOT NULL DEFAULT '0',
      `title` varchar(256) NOT NULL DEFAULT '',
      `content` text NOT NULL,
      `item_list` varchar(256) NOT NULL DEFAULT '[]',
      `rate` int(11) NOT NULL DEFAULT '0',
      PRIMARY KEY (`channel`,`stage`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8;"
    
  .

tabs() -> 
    [ 
    account,role,item,economy
        ].


not_init_tab() ->
    [account,guild_member,arena,vip,dun_trial_tower,task,dun_dark_tower,dun_trial_tower,picture_head,invite_code,town].


%%----------------------------map data-------------------------------------------

map(account) ->
    #map{
        fields          = record_info(fields, account),
        key_fields      = [id],
        string_fields   = [account,nick_name,reg_ip,login_ip,suid,platform_name,introduction],
        term_fields     = []
    };



map(role) ->
    #map{
        fields          = record_info(fields, role),
        key_fields      = [account_id,world_id],
        term_fields     = [skills,equips,attris,base_attris],
        fields_length   = [{skills,512}],
        ignored_fields  = [battle_power]
    };

map(item) ->
    #map{
        fields          = record_info(fields, item),
        key_fields      = [account_id,world_id],
        term_fields     = [random_attr],
        fields_length   = [{skills,512}]
    };


map(economy) ->
    #map{
        fields          = record_info(fields, economy),
        key_fields      = [account_id],
        term_fields     = []
    }.





%%--------------------------------------------------------------------------------

cache_opt(rank) -> #cache_opt{
    pre_load = true
};

cache_opt(account) -> #cache_opt{
    pre_load = true
};

cache_opt(guild) -> #cache_opt{
    pre_load = true
};

cache_opt(guild_application) -> #cache_opt{
    pre_load = true
};

cache_opt(guild_member) -> #cache_opt{
    pre_load = true
};

cache_opt(sys_mail) -> #cache_opt{
    pre_load = true
};

cache_opt(activity) -> #cache_opt{
    pre_load = true
};

% cache_opt(arena_rank) -> #cache_opt{
%     pre_load = true
% };

cache_opt(vip_month_card) -> #cache_opt{
    pre_load = true
};

cache_opt(private_chat) -> #cache_opt{
    pre_load = true
};

cache_opt(bulletin) -> #cache_opt{
    pre_load = true
};

cache_opt(activity_tl_tot_recharge_cfg) -> #cache_opt{
    pre_load = true
};

cache_opt(activity_tl_single_recharge_cfg) -> #cache_opt{
    pre_load = true
};

cache_opt(activity_tl_tot_consume_cfg) -> #cache_opt{
    pre_load = true
};

cache_opt(activity_last) -> #cache_opt{
    need_gc = false
};

cache_opt(activity_tl_tot_recharge_2_cfg) -> #cache_opt{
    pre_load = true
};

cache_opt(activity_tl_tot_consume_2_cfg) -> #cache_opt{
    pre_load = true
};

cache_opt(activity_tl_tot_recharge_daily_cfg) -> #cache_opt{
    pre_load = true
};

cache_opt(activity_tl_tot_consume_daily_cfg) -> #cache_opt{
    pre_load = true
};

cache_opt(activity_tl_tot_recharge_daily_2_cfg) -> #cache_opt{
    pre_load = true
};

cache_opt(activity_tl_tot_consume_daily_2_cfg) -> #cache_opt{
    pre_load = true
};

cache_opt(activity_tl_diamond_call_role_cfg) -> #cache_opt{
    pre_load = true
};

cache_opt(activity_tl_login_cfg) -> #cache_opt{
    pre_load = true
};

cache_opt(activity_tl_con_login_cfg) -> #cache_opt{
    pre_load = true
};

cache_opt(activity_tl_recharge_diamond_rank_cfg) -> #cache_opt{
    pre_load = true
};

cache_opt(activity_tl_consume_diamond_rank_cfg) -> #cache_opt{
    pre_load = true
};

cache_opt(activity_tl_cross_recharge_diamond_rank_cfg) -> #cache_opt{
    pre_load = true
};

cache_opt(activity_tl_cross_consume_diamond_rank_cfg) -> #cache_opt{
    pre_load = true
};

cache_opt(activity_tl_cross_daily_recharge_diamond_rank_cfg) -> #cache_opt{
    pre_load = true
};

cache_opt(activity_tl_cross_daily_consume_diamond_rank_cfg) -> #cache_opt{
    pre_load = true
};

cache_opt(_) -> 
    {ok, Is_pre_all} = application:get_env(cache, is_pre_all),
    case Is_pre_all of
        true ->
            #cache_opt {pre_load = true};
        false ->
            #cache_opt {pre_load = false}
    end.

