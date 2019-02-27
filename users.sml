(* ChuangWei Ma*)
(* 110862479 *)
(* CSE307 HW_2*)

val User = "Ellen"
val ListUserRoleTuples = [("John", "CEO"), ("John", "chair"), ("Ellen", "Secretary")];
val ListRoleRoleTuples = [("CEO", "IT"),("IT", "SEC")]
val UserAuthorizedRoles = ["CEO", "IT", "SEC"];
val ListRolePermissionTuples = [("CEO", "BUDGET"), ("IT", "FILE SYSTEM"), ("SEC", "DOC_1"),
									("SEC", "DOC_2")];


fun remove(x,L) = 
	if (L=[]) then []
	else if x=hd(L)then remove(x,tl(L))
	else hd(L)::remove(x,tl(L));

fun removedupl(L) =
	if (L=[]) then []
	else hd(L)::removedupl(remove(hd(L),tl(L)));


fun immediateRoles(User, ListUserRoleTuples : (''a * ''a) list) = 
	if ListUserRoleTuples = [] then []
	else if User = #1(hd ListUserRoleTuples) 
	then #2(hd ListUserRoleTuples) :: immediateRoles(User, tl ListUserRoleTuples)
	else immediateRoles(User, tl ListUserRoleTuples);

fun transitiveRoles(Role, ListRoleRoleTuples : (''a * ''a) list) =
	if ListRoleRoleTuples = [] then []
	else if Role = #1(hd ListRoleRoleTuples)
	then #2(hd ListRoleRoleTuples) :: transitiveRoles(Role, tl ListRoleRoleTuples)
	else transitiveRoles(Role, tl ListRoleRoleTuples);


fun accumulateRoles(Res, ImmediateRoles, ListRoleRoleTuples : (''a * ''a) list) =
	if ImmediateRoles = [] then Res
	else accumulateRoles(hd ImmediateRoles :: Res, tl(ImmediateRoles @ transitiveRoles(hd ImmediateRoles ,ListRoleRoleTuples)), ListRoleRoleTuples);
	

fun authorizedRoles(User, ListUserRoleTuples, ListRoleRoleTuples : (''a * ''a) list) = 
	removedupl(accumulateRoles([], immediateRoles(User, ListUserRoleTuples), ListRoleRoleTuples));

authorizedRoles(1, [(1,1), (1,2)], [(1,2), (1,3), (2,4)]);
authorizedRoles(User, ListUserRoleTuples, ListRoleRoleTuples);



fun accumulatePermissions(Role, ListRolePermissionTuples : (''a * ''b) list) = 
	if ListRolePermissionTuples = [] then []
	else if Role = #1(hd ListRolePermissionTuples)
	then #2(hd ListRolePermissionTuples) :: accumulatePermissions(Role, tl ListRolePermissionTuples)
	else accumulatePermissions(Role, tl ListRolePermissionTuples);

fun authorizedPermissions(User, UserAuthorizedRoles, ListRolePermissionTuples : (''a * ''b) list) = 
	if UserAuthorizedRoles = [] then []
	else removedupl(accumulatePermissions(hd UserAuthorizedRoles, ListRolePermissionTuples) @ 
		authorizedPermissions(User, tl UserAuthorizedRoles, ListRolePermissionTuples));


authorizedPermissions(User, UserAuthorizedRoles, ListRolePermissionTuples);

authorizedPermissions(1, [1,2,3,4], [(1,2), (1,3), (2,2)]);









