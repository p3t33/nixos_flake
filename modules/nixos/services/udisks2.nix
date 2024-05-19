{ ... }:
{
     # This service handles auto mount of devices such as thumb drives by the user(with cli command that I start
     # on i3 login), no root requried. it is only required for something bare
     # bones such as i4.
     services.udisks2.enable = true;
}
