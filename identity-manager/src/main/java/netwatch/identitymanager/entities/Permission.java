package netwatch.identitymanager.entities;

import jakarta.persistence.EmbeddedId;
import jakarta.persistence.Entity;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

// This class represents the "permissions" entity in the database.
// The Primary Key is the composite field "id" defined by the Embedded class {@link PermissionId}. It includes the role name, method, and object.
// The "role" field is a Many-to-One relationship to the "name" column in the {@link Role} entity.
// @author Antonio Scardace
// @version 1.0

@NoArgsConstructor
@AllArgsConstructor
@Data
@Entity
@Table(name = "permissions")
public class Permission {

    @EmbeddedId
    private PermissionId id;

    @ManyToOne
	@JoinColumn(name = "role", referencedColumnName = "name", insertable = false, updatable = false)
    private Role role;
}