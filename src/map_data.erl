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

map(rank) ->
    #map{
        fields          = record_info(fields, rank),
        key_fields      = [type,id]
        % string_fields   = [md5]
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

map(counter) ->
    #map{
        fields          = record_info(fields, counter),
        key_fields      = [account_id,world_id],
        term_fields     = []
    };

map(counter_activity) ->
    #map{
        fields          = record_info(fields, counter_activity),
        key_fields      = [account_id,activity_id],
        term_fields     = []
    };

map(per_counter) ->
    #map{
        fields          = record_info(fields, per_counter),
        key_fields      = [account_id,world_id],
        term_fields     = []
    };  

map(list_counter) ->
    #map{
        fields          = record_info(fields, list_counter),
        key_fields      = [account_id,world_id],
        term_fields     = [counter],
        fields_length   = [{counter,2048}]
    };

map(system) ->
    #map{
        fields          = record_info(fields, system),
        key_fields      = [account_id,type],
        term_fields     = []
    };

map(system_list) ->
    #map{
        fields          = record_info(fields, system_list),
        key_fields      = [account_id,type],
        term_fields     = [list]
    };

map(role_call) ->
    #map{
        fields          = record_info(fields, role_call),
        key_fields      = [account_id,type],
        term_fields     = []
    };

map(economy) ->
    #map{
        fields          = record_info(fields, economy),
        key_fields      = [account_id],
        term_fields     = []
    };

map(cd) ->
     #map{
        fields          = record_info(fields, cd),
        key_fields      = [account_id,type],
        term_fields     = []
    };

map(friend) ->
    #map{
        fields          = record_info(fields,friend),
        key_fields      = [account_id,target_id],
        term_fields     = []
    };

map(friend_apply) ->
    #map{
        fields          = record_info(fields,friend_apply),
        key_fields      = [account_id,target_id],
        term_fields     = []
    };

map(mail) ->
    #map{
        fields          = record_info(fields, mail),
        key_fields      = [account_id,mail_id],
        term_fields     = [attachment_items,attachment_economys],
        string_fields   = [mail_receiver_nick_name,mail_sender_nick_name,mail_title,mail_content],
        fields_length   = [{mail_content,1024},{attachment_items, 1024}]
    };

map(sys_mail) ->
    #map{
        fields          = record_info(fields, sys_mail),
        key_fields      = [sys_mail_id],
        term_fields     = [attachment_items],
        string_fields   = [mail_title,mail_content],
        fields_length   = [{mail_content,1024},{attachment_items, 1024}]
    };

map(town) ->
    #map{
        fields          = record_info(fields, town),
        key_fields      = [account_id],
        term_fields     = [open_list],
        fields_length   = [{open_list,2048}]
    };

map(town_obj) ->
     #map{
        fields          = record_info(fields, town_obj),
        key_fields      = [account_id,world_id],
        term_fields     = []
    };

map(town_monster) ->
     #map{
        fields          = record_info(fields, town_monster),
        key_fields      = [account_id,drop_id],
        term_fields     = []
    };

map(town_award_box) ->
     #map{
        fields          = record_info(fields, town_award_box),
        key_fields      = [account_id],
        term_fields     = [award],
        fields_length   = [{award,4096}]
    };


map(guild) ->
    #map{
         fields         = record_info(fields, guild),
         key_fields     = [id],
         string_fields  = [name,notice],
         ignored_fields = [members]
    };

map(guild_application) ->
    #map{
         fields         = record_info(fields,guild_application),
         key_fields     = [guild_id,account_id]      
    };

map(guild_member) ->
    #map{
         fields         = record_info(fields, guild_member),
         key_fields     = [account_id]
    };

map(private_chat) ->
    #map{
         fields         = record_info(fields, private_chat),
         key_fields     = [account_smaller, account_larger, counter],
         string_fields  = [content]
    };

map(private_chat_counter) ->
    #map{
         fields         = record_info(fields, private_chat_counter),
         key_fields     = [account_smaller, account_larger]
    };

map(guild_chat) ->
    #map{
         fields         = record_info(fields, guild_chat),
         key_fields     = [guild_id, counter],
         string_fields  = [content]
    };

map(guild_chat_counter) ->
    #map{
         fields         = record_info(fields, guild_chat_counter),
         key_fields     = [guild_id]
    };

map(world_chat) ->
    #map{
         fields         = record_info(fields, world_chat),
         key_fields     = [world_id, counter],
         string_fields  = [content]
    };

map(world_chat_counter) ->
    #map{
         fields         = record_info(fields, world_chat_counter),
         key_fields     = [world_id]
    };

map(myst_shop) ->
    #map{
         fields         = record_info(fields, myst_shop),
     term_fields = [items,vip_items],
         key_fields     = [account_id]
    };

map(activity) ->
    #map{
         fields          = record_info(fields, activity),
         key_fields      = [activity_id],
         term_fields     = [cycle_begin_time,match_time_list,match_week_list,match_date_list,cycle_end_time,award_item_list],
         string_fields   = [award_desc,content]
    };

map(activity_last) ->
    #map{
         fields          = record_info(fields, activity_last),
         key_fields      = [activity_id]
    };

map(activity_login) ->
    #map{
         fields          = record_info(fields, activity_login),
         key_fields      = [account_id,activity_id]
    };

map(activity_daily) ->
    #map{
         fields          = record_info(fields, activity_daily),
         key_fields      = [account_id,activity_id]
    };

map(activity_tl_manytimes_recharge) ->
    #map{
         fields          = record_info(fields, activity_tl_manytimes_recharge),
         key_fields      = [account_id],
         term_fields     = [recharge_diamond_list]
    };

map(activity_tl_tot_recharge) ->
    #map{
         fields          = record_info(fields, activity_tl_tot_recharge),
         key_fields      = [account_id],
         term_fields     = [award_stage_list]
    };

map(activity_tl_tot_recharge_cfg) ->
    #map{
         fields          = record_info(fields, activity_tl_tot_recharge_cfg),
         key_fields      = [stage],
         term_fields     = [item_list]
    };

map(activity_tl_tot_recharge_2) ->
    #map{
         fields          = record_info(fields, activity_tl_tot_recharge_2),
         key_fields      = [account_id],
         term_fields     = [award_stage_list]
    };

map(activity_tl_tot_recharge_2_cfg) ->
    #map{
         fields          = record_info(fields, activity_tl_tot_recharge_2_cfg),
         key_fields      = [stage],
         term_fields     = [item_list]
    };

map(activity_tl_single_recharge) ->
    #map{
         fields          = record_info(fields, activity_tl_single_recharge),
         key_fields      = [account_id],
         term_fields     = [award_stage_list]
    };

map(activity_tl_single_recharge_cfg) ->
    #map{
         fields          = record_info(fields, activity_tl_single_recharge_cfg),
         key_fields      = [stage],
         term_fields     = [item_list]
    };

map(activity_tl_tot_consume) ->
    #map{
         fields          = record_info(fields, activity_tl_tot_consume),
         key_fields      = [account_id],
         term_fields     = [award_stage_list]
    };

map(activity_tl_tot_consume_cfg) ->
    #map{
         fields          = record_info(fields, activity_tl_tot_consume_cfg),
         key_fields      = [stage],
         term_fields     = [item_list]
    };

map(activity_tl_tot_consume_2) ->
    #map{
         fields          = record_info(fields, activity_tl_tot_consume_2),
         key_fields      = [account_id],
         term_fields     = [award_stage_list]
    };

map(activity_tl_tot_consume_2_cfg) ->
    #map{
         fields          = record_info(fields, activity_tl_tot_consume_2_cfg),
         key_fields      = [stage],
         term_fields     = [item_list]
    };




map(activity_tl_tot_recharge_daily) ->
    #map{
         fields          = record_info(fields, activity_tl_tot_recharge_daily),
         key_fields      = [account_id],
         term_fields     = [award_stage_list]
    };

map(activity_tl_tot_recharge_daily_cfg) ->
    #map{
         fields          = record_info(fields, activity_tl_tot_recharge_daily_cfg),
         key_fields      = [stage],
         term_fields     = [item_list]
    };

map(activity_tl_tot_recharge_daily_2) ->
    #map{
         fields          = record_info(fields, activity_tl_tot_recharge_daily_2),
         key_fields      = [account_id],
         term_fields     = [award_stage_list]
    };

map(activity_tl_tot_recharge_daily_2_cfg) ->
    #map{
         fields          = record_info(fields, activity_tl_tot_recharge_daily_2_cfg),
         key_fields      = [stage],
         term_fields     = [item_list]
    };


map(activity_tl_tot_consume_daily) ->
    #map{
         fields          = record_info(fields, activity_tl_tot_consume_daily),
         key_fields      = [account_id],
         term_fields     = [award_stage_list]
    };

map(activity_tl_tot_consume_daily_cfg) ->
    #map{
         fields          = record_info(fields, activity_tl_tot_consume_daily_cfg),
         key_fields      = [stage],
         term_fields     = [item_list]
    };

map(activity_tl_tot_consume_daily_2) ->
    #map{
         fields          = record_info(fields, activity_tl_tot_consume_daily_2),
         key_fields      = [account_id],
         term_fields     = [award_stage_list]
    };

map(activity_tl_tot_consume_daily_2_cfg) ->
    #map{
         fields          = record_info(fields, activity_tl_tot_consume_daily_2_cfg),
         key_fields      = [stage],
         term_fields     = [item_list]
    };

map(activity_tl_diamond_call_role_cfg) ->
    #map{
         fields          = record_info(fields, activity_tl_diamond_call_role_cfg),
         key_fields      = [stage],
         term_fields     = [item_list]
    };

map(activity_tl_login) ->
    #map{
         fields          = record_info(fields, activity_tl_login),
         key_fields      = [account_id],
         term_fields     = [award_stage_list]
    };

map(activity_tl_login_cfg) ->
    #map{
         fields          = record_info(fields, activity_tl_login_cfg),
         key_fields      = [stage],
         term_fields     = [item_list]
    };

map(activity_tl_con_login) ->
    #map{
         fields          = record_info(fields, activity_tl_con_login),
         key_fields      = [account_id],
         term_fields     = [award_stage_list]
    };

map(activity_tl_con_login_cfg) ->
    #map{
         fields          = record_info(fields, activity_tl_con_login_cfg),
         key_fields      = [stage],
         term_fields     = [item_list]
    };


map(activity_tl_recharge_diamond_rank_cfg) ->
    #map{
         fields          = record_info(fields, activity_tl_recharge_diamond_rank_cfg),
         key_fields      = [stage],
         term_fields     = [item_list]
    };

map(activity_tl_consume_diamond_rank_cfg) ->
    #map{
         fields          = record_info(fields, activity_tl_consume_diamond_rank_cfg),
         key_fields      = [stage],
         term_fields     = [item_list]
    };

map(activity_tl_cross_recharge_diamond_rank_cfg) ->
    #map{
         fields          = record_info(fields, activity_tl_cross_recharge_diamond_rank_cfg),
         key_fields      = [stage],
         term_fields     = [item_list]
    };

map(activity_tl_cross_consume_diamond_rank_cfg) ->
    #map{
         fields          = record_info(fields, activity_tl_cross_consume_diamond_rank_cfg),
         key_fields      = [stage],
         term_fields     = [item_list]
    };

map(activity_tl_cross_daily_recharge_diamond_rank_cfg) ->
    #map{
         fields          = record_info(fields, activity_tl_cross_daily_recharge_diamond_rank_cfg),
         key_fields      = [stage],
         term_fields     = [item_list]
    };

map(activity_tl_cross_daily_consume_diamond_rank_cfg) ->
    #map{
         fields          = record_info(fields, activity_tl_cross_daily_consume_diamond_rank_cfg),
         key_fields      = [stage],
         term_fields     = [item_list]
    }; 

map(activity_tl_invest) ->
    #map{
         fields          = record_info(fields, activity_tl_invest),
         key_fields      = [account_id, type],
         term_fields     = [award_stage_list]
    };

map(activity_tl_new_server) ->
    #map{
         fields          = record_info(fields, activity_tl_new_server),
         key_fields      = [account_id, new_server_id]
    };

map(jyukai) ->
    #map{
         fields          = record_info(fields, jyukai),
         key_fields      = [account_id,undertown_id],
         term_fields     = [role_list]
    };

map(picture) ->
    #map{
         fields          = record_info(fields, picture),
         key_fields      = [account_id],
         term_fields     = [role_list,monster_list],
         fields_length   = [{role_list,8192},{monster_list,8192}]
    };

map(picture_head) ->
    #map{
         fields          = record_info(fields, picture_head),
         key_fields      = [account_id],
         term_fields     = [base_list,special_list,frame_list,nickname_frame_list],
         fields_length   = [{base_list,5120},{special_list,5120},{frame_list,5120},{nickname_frame_list,5120}]
    };

map(achieve) ->
    #map{
         fields          = record_info(fields, achieve),
         key_fields      = [account_id, achieve_id]
    };

map(battle_heros) ->
    #map{
        fields          = record_info(fields, battle_heros),
        key_fields      = [account_id, battle_type],
        term_fields     = [hero_ids,assistants_ids]
    };

map(dun) ->
    #map{
        fields          = record_info(fields, dun),
        key_fields      = [account_id, dun_id]
    };

map(dun_reward)->
    #map{
        fields          = record_info(fields, dun_reward),
        key_fields      = [account_id, chapter_id, progress]
    };

map(role_train) ->
    #map{
         fields          = record_info(fields, role_train),
         key_fields      = [account_id, train_slot_id]
    };

map(role_train_potion) ->
    #map{
         fields          = record_info(fields, role_train_potion),
         key_fields      = [account_id, potion_id]
    };

map(battle) ->
    #map{
        fields          = record_info(fields, battle),
        key_fields      = [account_id],
        string_fields   = [battle_key]
    };

map(rand_dun) ->
    #map{
        fields          = record_info(fields, rand_dun),
        key_fields      = [account_id, chapter_id, dun_id]
    };

map(dun_trial_tower) ->
    #map{
        fields          = record_info(fields, dun_trial_tower),
        key_fields      = [account_id]
    };

map(role_potential) ->
    #map{
        fields          = record_info(fields, role_potential),
        key_fields      = [account_id, world_id, type]
    };

map(recharge_info) ->
    #map{
        fields          = record_info(fields, recharge_info),
        key_fields      = [inner_recharge_id],
        string_fields   = [suid,recharge_item]
    };

map(appstore_order) ->
    #map{
        fields          = record_info(fields, appstore_order),
        key_fields      = [transaction_id],
        string_fields   = [product_id, receipt]
    };

map(award_login) ->
    #map{
        fields          = record_info(fields, award_login),
        key_fields      = [account_id],
        term_fields   = [award_list]
    };

map(arena) ->
    #map{
        fields          = record_info(fields, arena),
        key_fields      = [account_id],
        term_fields   = [role_list,report_list]
    };

% map(arena_rank) ->
%     #map{
%         fields          = record_info(fields, arena_rank),
%         key_fields      = [account_id]
%     };

map(vip) ->
    #map{
        fields          = record_info(fields, vip),
        key_fields      = [account_id],
        term_fields   = [gift_list]
    };

map(vip_privilege) ->
    #map{
        fields          = record_info(fields, vip_privilege),
        key_fields      = [account_id, type]
    };

map(vip_month_card) ->
    #map{
        fields          = record_info(fields, vip_month_card),
        key_fields      = [account_id]
    };

map(guild_invitation) ->
    #map{
        fields      = record_info(fields, guild_invitation),
        key_fields  = [account_id, guild_id]
    };

map(guild_event) ->
    #map{
        fields      = record_info(fields, guild_event),
        key_fields  = [guild_id, event_id],
        string_fields = [log]
    };

map(dun_dark_tower) ->
    #map{
        fields      = record_info(fields, dun_dark_tower),
        key_fields  = [account_id],
        term_fields = [cell_list,role_list,boss_list,buff_list,award_list],
        fields_length = [{cell_list,1024},{boss_list,1024},{role_list,2048}]
    };

map(task) ->
    #map{
        fields      = record_info(fields, task),
        key_fields  = [account_id, task_id]
    };

map(task_branch) ->
    #map{
        fields      = record_info(fields, task_branch),
        key_fields  = [account_id, task_id]
    };

map(task_reward) ->
    #map{
        fields      = record_info(fields, task_reward),
        key_fields  = [account_id, task_id]
    };

map(guide) ->
    #map{
        fields      = record_info(fields, guide),
        key_fields  = [account_id],
        term_fields = [guide_list],
        fields_length = [{guide_list,4096}]
    };


map(guild_building) ->
    #map{
        fields      = record_info(fields, guild_building),
        key_fields  = [guild_id, type]
    };

map(clockin) ->
    #map{
        fields      = record_info(fields, clockin),
        key_fields  = [account_id]
    };

map(shop) ->
    #map{
        fields      = record_info(fields, shop),
        key_fields  = [account_id, id]
    };

map(bulletin) ->
    #map{
        fields      = record_info(fields, bulletin),
        key_fields  = [seq_id],
        string_fields = [content]
    };

map(invite_code) ->
    #map{
        fields      = record_info(fields, invite_code),
        key_fields  = [account_id],
        term_fields = [invited_list,invited_award_list],
        string_fields = [invite_code]
    };

map(sevenday) ->
  #map{
    fields      = record_info(fields, sevenday),
    key_fields  = [account_id,times]
  };

map(fb_invite_friends) ->
  #map{
    fields      = record_info(fields, fb_invite_friends),
    key_fields  = [account_id],
    term_fields = [friends,awards]
  };

map(question) ->
  #map{
    fields      = record_info(fields, question),
    key_fields  = [account_id, id],
    string_fields = [msg]
  };

map(award_growup)->
  #map{
    fields      = record_info(fields, award_growup),
    key_fields  = [account_id, id]
  };

map(mercenary)->
  #map{
    fields      = record_info(fields, mercenary),
    term_fields = [hired_mercenary],
    key_fields  = [account_id]
  };

map(hired_mercenary)->
  #map{
    fields     = record_info(fields, hired_mercenary),
    key_fields = [account_id, pos],
    string_fields = [role_info]
  };

map(guild_dun) ->
    #map{
    fields     = record_info(fields, guild_dun),
    key_fields = [guild_id,charper_id],
    term_fields = [members_rank,monsters,members_times],
    fields_length = [{members_rank,2048},{monsters,2048},{members_times,2048}]
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

