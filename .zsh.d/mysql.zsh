# -------------------------------------------------------
# see http://d.hatena.ne.jp/Tetsujin/20120815/1345033377
# -------------------------------------------------------

# MySQLの接続元ホスト名($HOST)によって色分けします。
# - .local.を含むローカル環境は緑
# - .dev.を含む開発環境では黄色
# - その他は赤
# としています。
typeset -A mysql_prompt_style_client_host
mysql_prompt_style_client_host=(
    '*.local.*'     "$fg_bold[green]"
    '*.dev.*'       "$fg_bold[yellow]"
    '*'             "$fg_bold[red]"
)
# MySQLの接続先ホストのユーザ名(-uオプション)によって色分けします。
# - rootは赤
# - その他は青
# としています。
typeset -A mysql_prompt_style_server_user
mysql_prompt_style_server_user=(
    'root'          "$bg_bold[red]$fg_bold[yellow]"
    '*'             "$fg_bold[blue]"
)
# MySQLの接続先ホスト名(-hオプション)によって色分けします。
# - masterを含む場合は赤
# - slaveを含む場合は黄色
# - その他は青色
# としています。
typeset -A mysql_prompt_style_server_host
mysql_prompt_style_server_host=(
    '*master*'      "$bg_bold[red]$fg_bold[yellow]"  # Master Server
    '*slave*'       "$bg[yellow]$fg[black]" # Slvae Server
    '*'             "$fg_bold[blue]"
)

# プロンプトを組み立てます。
# [注意] functionの実行直前に変数を評価するのでシングルクォートで定義してください。
mysql_prompt='${style_client_host}${USER}@${HOST}${fg_bold[white]} -> '
mysql_prompt=$mysql_prompt'${style_server_user}\u${reset_color}${fg_bold[white]}@${style_server_host}\h${reset_color}${fg_bold[white]}:${fg[magenta]}\d ${fg_bold[white]}\v\n'
mysql_prompt=$mysql_prompt'${fg_bold[white]}${bg_level}mysql${reset_color}> '